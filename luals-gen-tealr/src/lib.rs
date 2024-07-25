use tealr::{mlu::TealData, FunctionParam, KindOfType, Name, TealMultiValue, ToTypename};

trait TealrLuaLsGen {
    fn generate();
}

pub struct Generator<T: std::io::Write> {
    obj_name: String,
    writer: T,
}

/// rewrite of tealr's `new_type_to_old`
fn tealr_to_luals(a: tealr::Type, w: &mut impl std::io::Write) -> std::io::Result<()> {
    use tealr::{FunctionRepresentation, MapRepresentation, NamePart, TealType, Type};
    match a {
        Type::Single(a) => write!(
            w,
            "{}",
            NamePart::Type(TealType {
                name: a.name.0,
                type_kind: a.kind,
                generics: None,
            })
        )?,
        Type::Array(x) => {
            tealr_to_luals(*x, w)?;
            write!(w, "[]")?;
        }
        Type::Map(MapRepresentation { key, value }) => {
            write!(w, "{{")?;
            tealr_to_luals(*key, w)?;
            write!(w, " : ")?;
            tealr_to_luals(*value, w)?;
            write!(w, "}}")?;
        }
        Type::Or(mut x) => {
            write!(w, "(")?;
            let last = x.pop();
            for part in x {
                tealr_to_luals(part, w)?;
                write!(w, " | ")?;
            }

            if let Some(last) = last {
                tealr_to_luals(last, w)?;
            }

            write!(w, ")")?;
        }
        Type::Tuple(mut x) => {
            write!(w, "(")?;
            let last = x.pop();
            for part in x {
                tealr_to_luals(part, w)?;
                write!(w, " , ")?;
            }

            if let Some(last) = last {
                tealr_to_luals(last, w)?;
            }

            write!(w, ")")?;
        }
        Type::Function(FunctionRepresentation {
            mut params,
            mut returns,
        }) => {
            write!(w, "fun(")?;

            let last_param = params.pop();
            let param_count = params.len();
            for (i, param) in params.into_iter().enumerate() {
                write_function_param(param, i as u8, w)?;
                write!(w, " , ")?;
            }

            if let Some(p) = last_param {
                write_function_param(p, param_count as u8, w)?;
            }

            write!(w, ")")?;
            if !returns.is_empty() {
                write!(w, ": (")?;
                let last_return = returns.pop();
                for ret in returns {
                    tealr_to_luals(ret, w)?;
                    write!(w, " , ")?;
                }

                if let Some(ret) = last_return {
                    tealr_to_luals(ret, w)?;
                }

                write!(w, ")")?;
            }
        }
    }
    Ok(())
}

fn write_function_param(
    p: FunctionParam,
    index: u8,
    w: &mut impl std::io::Write,
) -> std::io::Result<()> {
    let name = get_param_name(p.param_name.as_ref(), index);
    write!(w, "{}: ", name)?;
    tealr_to_luals(p.ty, w)?;
    Ok(())
}

fn is_nil(t: &tealr::Type) -> bool {
    match t {
        tealr::Type::Single(s) => s.name.0.eq("nil") && s.kind == KindOfType::Builtin,
        _ => false,
    }
}

fn is_optional(t: &tealr::Type) -> bool {
    match t {
        tealr::Type::Or(t) => t.iter().any(is_nil),
        _ => false,
    }
}

impl<T: std::io::Write> Generator<T> {
    fn write_func<A: TealMultiValue, R: TealMultiValue>(
        &mut self,
        func_name: impl AsRef<str>,
        method: bool,
    ) -> std::io::Result<()> {
        for (i, p) in A::get_types_as_params().into_iter().enumerate() {
            write!(
                self.writer,
                "---@param {}{} ",
                get_param_name(p.param_name.as_ref(), i as u8),
                if is_optional(&p.ty) { "?" } else { "" }
            )?;
            tealr_to_luals(p.ty, &mut self.writer)?;
            writeln!(self.writer)?;
        }

        let mut returns = R::get_types();

        if !returns.is_empty() {
            write!(self.writer, "---@return (")?;
            let last_return = returns.pop();

            for ty in returns {
                tealr_to_luals(ty, &mut self.writer)?;
                write!(self.writer, " , ")?;
            }

            if let Some(ty) = last_return {
                tealr_to_luals(ty, &mut self.writer)?;
            }

            write!(self.writer, ")")?;
            writeln!(self.writer)?;
        }

        writeln!(
            self.writer,
            "function {}{}{}({}) end",
            &self.obj_name,
            if method { ':' } else { '.' },
            func_name.as_ref(),
            A::get_types_as_params()
                .into_iter()
                .enumerate()
                .map(|(i, p)| get_param_name(p.param_name.as_ref(), i as u8))
                .collect::<Vec<_>>()
                .join(", ")
        )?;

        Ok(())
    }

    fn write_field<F: ToTypename>(&mut self, n: impl AsRef<str>) -> std::io::Result<()> {
        write!(self.writer, "---@field {} ", n.as_ref())?;
        tealr_to_luals(F::to_typename(), &mut self.writer)?;
        writeln!(self.writer)
    }

    pub fn write_type<S: TealData>(name: impl Into<String>, writer: T) -> std::io::Result<()> {
        let mut s = Self {
            obj_name: name.into(),
            writer,
        };

        //---@class {name}
        write!(s.writer, "---@class ")?;
        tealr_to_luals(S::to_typename(), &mut s.writer)?;
        writeln!(s.writer)?;

        S::add_fields(&mut s);

        writeln!(s.writer, "{} = {{ }}", s.obj_name)?;

        S::add_methods(&mut s);
        writeln!(s.writer)
    }
}

fn get_param_name(name: Option<&Name>, param_index: u8) -> String {
    name.map(|x| x.0.clone().into_owned()).unwrap_or_else(|| {
        char::from_u32((b'a' + param_index) as u32)
            .unwrap_or('x')
            .to_string()
    })
}

impl<'lua, T, W> tealr::mlu::TealDataMethods<'lua, T> for Generator<W>
where
    T: ToTypename,
    W: std::io::Write,
{
    fn add_method<S, A, R, M>(&mut self, name: &S, method: M)
    where
        S: ?Sized + AsRef<str>,
        A: tealr::mlu::mlua::FromLuaMulti<'lua> + tealr::TealMultiValue,
        R: tealr::mlu::mlua::IntoLuaMulti<'lua> + tealr::TealMultiValue,
        M: 'static
            + tealr::mlu::MaybeSend
            + Fn(&'lua tealr::mlu::mlua::Lua, &T, A) -> tealr::mlu::mlua::Result<R>,
    {
        self.write_func::<A, R>(name, true);
    }

    fn add_method_mut<S, A, R, M>(&mut self, name: &S, method: M)
    where
        S: ?Sized + AsRef<str>,
        A: tealr::mlu::mlua::FromLuaMulti<'lua> + tealr::TealMultiValue,
        R: tealr::mlu::mlua::IntoLuaMulti<'lua> + tealr::TealMultiValue,
        M: 'static
            + tealr::mlu::MaybeSend
            + FnMut(&'lua tealr::mlu::mlua::Lua, &mut T, A) -> tealr::mlu::mlua::Result<R>,
    {
        self.write_func::<A, R>(name, true);
    }

    fn add_function<S, A, R, F>(&mut self, name: &S, function: F)
    where
        S: ?Sized + AsRef<str>,
        A: tealr::mlu::mlua::FromLuaMulti<'lua> + tealr::TealMultiValue,
        R: tealr::mlu::mlua::IntoLuaMulti<'lua> + tealr::TealMultiValue,
        F: 'static
            + tealr::mlu::MaybeSend
            + Fn(&'lua tealr::mlu::mlua::Lua, A) -> tealr::mlu::mlua::Result<R>,
    {
        self.write_func::<A, R>(name, false);
    }

    fn add_function_mut<S, A, R, F>(&mut self, name: &S, function: F)
    where
        S: ?Sized + AsRef<str>,
        A: tealr::mlu::mlua::FromLuaMulti<'lua> + tealr::TealMultiValue,
        R: tealr::mlu::mlua::IntoLuaMulti<'lua> + tealr::TealMultiValue,
        F: 'static
            + tealr::mlu::MaybeSend
            + FnMut(&'lua tealr::mlu::mlua::Lua, A) -> tealr::mlu::mlua::Result<R>,
    {
        self.write_func::<A, R>(name, false);
    }

    fn add_meta_method<A, R, M>(&mut self, meta: tealr::mlu::mlua::MetaMethod, method: M)
    where
        A: tealr::mlu::mlua::FromLuaMulti<'lua> + tealr::TealMultiValue,
        R: tealr::mlu::mlua::IntoLuaMulti<'lua> + tealr::TealMultiValue,
        M: 'static
            + tealr::mlu::MaybeSend
            + Fn(&'lua tealr::mlu::mlua::Lua, &T, A) -> tealr::mlu::mlua::Result<R>,
    {
    }

    fn add_meta_method_mut<A, R, M>(&mut self, meta: tealr::mlu::mlua::MetaMethod, method: M)
    where
        A: tealr::mlu::mlua::FromLuaMulti<'lua> + tealr::TealMultiValue,
        R: tealr::mlu::mlua::IntoLuaMulti<'lua> + tealr::TealMultiValue,
        M: 'static
            + tealr::mlu::MaybeSend
            + FnMut(&'lua tealr::mlu::mlua::Lua, &mut T, A) -> tealr::mlu::mlua::Result<R>,
    {
    }

    fn add_meta_function<A, R, F>(&mut self, meta: tealr::mlu::mlua::MetaMethod, function: F)
    where
        A: tealr::mlu::mlua::FromLuaMulti<'lua> + tealr::TealMultiValue,
        R: tealr::mlu::mlua::IntoLuaMulti<'lua> + tealr::TealMultiValue,
        F: 'static
            + tealr::mlu::MaybeSend
            + Fn(&'lua tealr::mlu::mlua::Lua, A) -> tealr::mlu::mlua::Result<R>,
    {
    }

    fn add_meta_function_mut<A, R, F>(&mut self, meta: tealr::mlu::mlua::MetaMethod, function: F)
    where
        A: tealr::mlu::mlua::FromLuaMulti<'lua> + tealr::TealMultiValue,
        R: tealr::mlu::mlua::IntoLuaMulti<'lua> + tealr::TealMultiValue,
        F: 'static
            + tealr::mlu::MaybeSend
            + FnMut(&'lua tealr::mlu::mlua::Lua, A) -> tealr::mlu::mlua::Result<R>,
    {
        todo!()
    }

    fn document(&mut self, documentation: &str) -> &mut Self {
        self
    }

    fn document_type(&mut self, documentation: &str) -> &mut Self {
        self
    }

    fn generate_help(&mut self) {}
}

impl<'lua, T, W> tealr::mlu::TealDataFields<'lua, T> for Generator<W>
where
    T: TealData,
    W: std::io::Write,
{
    fn document(&mut self, documentation: &str) {}

    fn add_field_method_get<S, R, M>(&mut self, name: &S, method: M)
    where
        S: AsRef<str> + ?Sized,
        R: tealr::mlu::mlua::IntoLua<'lua> + ToTypename,
        M: 'static
            + tealr::mlu::MaybeSend
            + Fn(&'lua tealr::mlu::mlua::Lua, &T) -> tealr::mlu::mlua::Result<R>,
    {
        self.write_field::<R>(name);
    }

    fn add_field_method_set<S, A, M>(&mut self, name: &S, method: M)
    where
        S: AsRef<str> + ?Sized,
        A: tealr::mlu::mlua::FromLua<'lua> + ToTypename,
        M: 'static
            + tealr::mlu::MaybeSend
            + FnMut(&'lua tealr::mlu::mlua::Lua, &mut T, A) -> tealr::mlu::mlua::Result<()>,
    {
    }

    fn add_field_function_get<S, R, F>(&mut self, name: &S, function: F)
    where
        S: AsRef<str> + ?Sized,
        R: tealr::mlu::mlua::IntoLua<'lua> + ToTypename,
        F: 'static
            + tealr::mlu::MaybeSend
            + Fn(
                &'lua tealr::mlu::mlua::Lua,
                tealr::mlu::mlua::AnyUserData<'lua>,
            ) -> tealr::mlu::mlua::Result<R>,
    {
        self.write_field::<R>(name);
    }

    fn add_field_function_set<S, A, F>(&mut self, name: &S, function: F)
    where
        S: AsRef<str> + ?Sized,
        A: tealr::mlu::mlua::FromLua<'lua> + ToTypename,
        F: 'static
            + tealr::mlu::MaybeSend
            + FnMut(
                &'lua tealr::mlu::mlua::Lua,
                tealr::mlu::mlua::AnyUserData<'lua>,
                A,
            ) -> tealr::mlu::mlua::Result<()>,
    {
    }

    fn add_meta_field_with<R, F>(&mut self, meta: tealr::mlu::mlua::MetaMethod, f: F)
    where
        F: 'static
            + tealr::mlu::MaybeSend
            + Fn(&'lua tealr::mlu::mlua::Lua) -> tealr::mlu::mlua::Result<R>,
        R: tealr::mlu::mlua::IntoLua<'lua> + ToTypename,
    {
    }
}
