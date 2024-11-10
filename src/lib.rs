use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
    fmt::Display,
    io::Write,
    path::{Path, PathBuf},
    rc::Rc,
    sync::Arc,
};

pub use luals_gen_derive::ToLuaLsType;

#[derive(Debug, Clone)]
pub enum LuaLsTypeDef {
    Func {
        args: Vec<(Cow<'static, str>, LuaLsType)>,
        output: Option<Box<LuaLsType>>,
    },
    Named(Cow<'static, str>),
    Union(Vec<LuaLsType>),
    Array(Box<LuaLsType>),
    Table {
        key: Box<LuaLsType>,
        value: Box<LuaLsType>,
    },
    TableLiteral(Vec<(Cow<'static, str>, LuaLsType)>),
}

#[derive(Debug, Clone)]
pub enum LuaLsType {
    Primitive(Cow<'static, str>),
    Named(Cow<'static, str>, LuaLsTypeDef),
    Class {
        name: Cow<'static, str>,
        fields: Vec<(Cow<'static, str>, LuaLsType)>,
    },
}

impl LuaLsType {
    pub fn name(&self) -> Cow<'static, str> {
        match self {
            LuaLsType::Primitive(n) | LuaLsType::Named(n, _) | LuaLsType::Class { name: n, .. } => {
                n.clone()
            }
        }
    }

    pub fn traverse_named(&self, types: &mut Vec<LuaLsType>, seen: &mut HashSet<String>) {
        match self {
            LuaLsType::Primitive(_) => {}
            LuaLsType::Named(n, t) => {
                if !seen.insert(n.clone().into_owned()) {
                    return;
                }
                types.push(LuaLsType::Named(n.clone(), t.clone()));
                match t {
                    LuaLsTypeDef::Named(_) => {}
                    LuaLsTypeDef::Union(v) => {
                        for ele in v {
                            ele.traverse_named(types, seen)
                        }
                    }
                    LuaLsTypeDef::Array(v) => v.traverse_named(types, seen),
                    LuaLsTypeDef::Table { key, value } => {
                        key.traverse_named(types, seen);
                        value.traverse_named(types, seen)
                    }
                    LuaLsTypeDef::TableLiteral(t) => {
                        for ele in t {
                            ele.1.traverse_named(types, seen)
                        }
                    }

                    LuaLsTypeDef::Func { args, output } => {
                        for ele in args {
                            ele.1.traverse_named(types, seen);
                        }
                        if let Some(r) = output {
                            r.traverse_named(types, seen);
                        }
                    }
                }
            }
            LuaLsType::Class { fields, name } => {
                if !seen.insert(name.clone().into_owned()) {
                    return;
                }

                types.push(LuaLsType::Class {
                    name: name.clone(),
                    fields: fields.clone(),
                });

                for ele in fields {
                    ele.1.traverse_named(types, seen);
                }
            }
        }
    }
}

impl LuaLsTypeDef {
    pub fn with_tag(self, tag: &'static str, id: &'static str) -> Self {
        match self {
            Self::TableLiteral(mut t) => {
                t.insert(
                    0,
                    (
                        tag.into(),
                        LuaLsType::Named(id.into(), Self::Named(id.into())),
                    ),
                );
                Self::TableLiteral(t)
            }
            Self::Named(name) => Self::TableLiteral(vec![(
                tag.into(),
                LuaLsType::Named(name.clone(), Self::Named(name)),
            )]),
            other => other,
        }
    }
}

pub trait ToLuaLsType {
    fn lua_ls_type() -> LuaLsType;
    fn lua_ls_type_name() -> Cow<'static, str>;
}

macro_rules! impl_ls_type {
    ($ls_type:literal $current_type:ty) => {
        impl ToLuaLsType for $current_type {
            fn lua_ls_type() -> LuaLsType {
                LuaLsType::Primitive($ls_type.into())
            }

            fn lua_ls_type_name() -> Cow<'static, str> {
                $ls_type.into()
            }
        }
    };
    ($ls_type:literal $current_type:ty,$($types:ty),*) => {
        impl_ls_type!($ls_type $current_type);
        impl_ls_type!($ls_type $($types),+);
    };
}

impl_ls_type!("boolean" bool);
impl_ls_type!("string" String,std::ffi::CString,&str,&std::ffi::CStr, Path, PathBuf);
impl_ls_type!("number" f32,f64);
impl_ls_type!("integer" i8,u8,u16,i16,u32,i32,u64,i64,u128,i128,isize,usize);

impl<T> ToLuaLsType for Vec<T>
where
    T: ToLuaLsType,
{
    fn lua_ls_type() -> LuaLsType {
        match T::lua_ls_type() {
            LuaLsType::Primitive(p) => LuaLsType::Primitive((p.into_owned() + "[]").into()),
            LuaLsType::Named(n, _) | LuaLsType::Class { name: n, .. } => {
                LuaLsType::Named(n + "[]", LuaLsTypeDef::Array(Box::new(T::lua_ls_type())))
            }
        }
    }

    fn lua_ls_type_name() -> Cow<'static, str> {
        if let LuaLsType::Named(n, _) | LuaLsType::Primitive(n) = T::lua_ls_type() {
            n
        } else {
            unreachable!()
        }
    }
}

impl<T> ToLuaLsType for HashSet<T>
where
    T: ToLuaLsType,
{
    fn lua_ls_type() -> LuaLsType {
        match T::lua_ls_type() {
            LuaLsType::Primitive(p) => LuaLsType::Primitive((p.into_owned() + "[]").into()),
            LuaLsType::Named(n, _) | LuaLsType::Class { name: n, .. } => {
                LuaLsType::Named(n + "[]", LuaLsTypeDef::Array(Box::new(T::lua_ls_type())))
            }
        }
    }

    fn lua_ls_type_name() -> Cow<'static, str> {
        if let LuaLsType::Named(n, _) | LuaLsType::Primitive(n) = T::lua_ls_type() {
            n
        } else {
            unreachable!()
        }
    }
}

impl<T> ToLuaLsType for Option<T>
where
    T: ToLuaLsType,
{
    fn lua_ls_type() -> LuaLsType {
        match T::lua_ls_type() {
            LuaLsType::Primitive(p) => LuaLsType::Named(
                p.clone() + "_optional",
                LuaLsTypeDef::Union(vec![
                    LuaLsType::Primitive(p),
                    LuaLsType::Primitive("nil".into()),
                ]),
            ),
            LuaLsType::Named(n, _) | LuaLsType::Class { name: n, .. } => LuaLsType::Named(
                n + "_optional",
                LuaLsTypeDef::Union(vec![T::lua_ls_type(), LuaLsType::Primitive("nil".into())]),
            ),
        }
    }

    fn lua_ls_type_name() -> Cow<'static, str> {
        let LuaLsType::Named(n, _) = T::lua_ls_type() else {
            unreachable!()
        };

        n
    }
}

impl<K, V> ToLuaLsType for HashMap<K, V>
where
    K: ToLuaLsType,
    V: ToLuaLsType,
{
    fn lua_ls_type() -> LuaLsType {
        let key = match K::lua_ls_type() {
            LuaLsType::Primitive(p) | LuaLsType::Class { name: p, .. } => {
                (p.clone(), LuaLsTypeDef::Named(p))
            }
            LuaLsType::Named(n, d) => (n.clone(), d),
        };

        let val = match V::lua_ls_type() {
            LuaLsType::Primitive(p) | LuaLsType::Class { name: p, .. } => {
                (p.clone(), LuaLsTypeDef::Named(p))
            }
            LuaLsType::Named(n, d) => (n.clone(), d),
        };

        LuaLsType::Named(
            format!("{}_{}_map", key.0, val.0).into(),
            LuaLsTypeDef::Table {
                key: Box::new(K::lua_ls_type()),
                value: Box::new(V::lua_ls_type()),
            },
        )
    }

    fn lua_ls_type_name() -> Cow<'static, str> {
        let LuaLsType::Named(n, _) = HashMap::<K, V>::lua_ls_type() else {
            unreachable!()
        };

        n
    }
}

impl<T, const N: usize> ToLuaLsType for [T; N]
where
    T: ToLuaLsType,
{
    fn lua_ls_type() -> LuaLsType {
        let val = match T::lua_ls_type() {
            LuaLsType::Primitive(p) | LuaLsType::Class { name: p, .. } => {
                (p.clone(), LuaLsTypeDef::Named(p))
            }
            LuaLsType::Named(n, d) => (n.clone(), d),
        };

        LuaLsType::Named(
            format!("{}_arr{}", val.0.clone(), N).into(),
            LuaLsTypeDef::TableLiteral(
                (1..=N)
                    .map(|i| (format!("[{i}]").into(), T::lua_ls_type()))
                    .collect(),
            ),
        )
    }

    fn lua_ls_type_name() -> Cow<'static, str> {
        let LuaLsType::Named(n, _) = T::lua_ls_type() else {
            unreachable!()
        };

        n
    }
}

impl<T> ToLuaLsType for Box<T>
where
    T: ToLuaLsType,
{
    fn lua_ls_type() -> LuaLsType {
        T::lua_ls_type()
    }

    fn lua_ls_type_name() -> Cow<'static, str> {
        T::lua_ls_type_name()
    }
}

impl<T> ToLuaLsType for Arc<T>
where
    T: ToLuaLsType,
{
    fn lua_ls_type() -> LuaLsType {
        T::lua_ls_type()
    }

    fn lua_ls_type_name() -> Cow<'static, str> {
        T::lua_ls_type_name()
    }
}

impl<T> ToLuaLsType for Rc<T>
where
    T: ToLuaLsType,
{
    fn lua_ls_type() -> LuaLsType {
        T::lua_ls_type()
    }

    fn lua_ls_type_name() -> Cow<'static, str> {
        T::lua_ls_type_name()
    }
}

pub struct LuaLsGen;

impl Display for LuaLsTypeDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use std::fmt::Write;
        match self {
            LuaLsTypeDef::Named(n) => f.write_str(n)?,
            LuaLsTypeDef::Union(u) => {
                f.write_char('(')?;
                for (i, v) in u.iter().enumerate() {
                    if i != 0 {
                        f.write_str(" | ")?;
                    }

                    f.write_fmt(format_args!("{}", v.name()))?;
                }
                f.write_char(')')?;
            }
            LuaLsTypeDef::Array(t) => f.write_fmt(format_args!("{}[]", t.name()))?,
            LuaLsTypeDef::Table { key, value } => {
                f.write_fmt(format_args!("table<{}, {}>", key.name(), value.name()))?
            }
            LuaLsTypeDef::TableLiteral(lit) => {
                f.write_str("{ ")?;
                for (i, kv) in lit.iter().enumerate() {
                    f.write_fmt(format_args!("{}: {}", kv.0, kv.1.name()))?;
                    if i < lit.len() - 1 {
                        f.write_str(", ")?;
                    }
                }
                f.write_str(" }")?;
            }

            LuaLsTypeDef::Func { args, output } => {
                //fun(name: string, value: any): boolean
                let args = args
                    .iter()
                    .map(|x| format!("{}: {}", x.0, x.1.name()))
                    .collect::<Vec<_>>()
                    .join(", ");
                let out = if let Some(out) = output {
                    format!(":{}", out.name())
                } else {
                    String::new()
                };
                f.write_fmt(format_args!("fun({args}){out}"))?;
            }
        }

        Ok(())
    }
}

impl Display for LuaLsType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LuaLsType::Primitive(n) | LuaLsType::Class { name: n, .. } => f.write_str(n),
            LuaLsType::Named(_, d) => f.write_fmt(format_args!("{d}")),
        }
    }
}

impl LuaLsGen {
    pub fn generate_types<T: ToLuaLsType>(mut w: impl Write) -> std::io::Result<()> {
        let t = T::lua_ls_type();
        let mut types = Vec::new();

        t.traverse_named(&mut types, &mut HashSet::new());

        types.sort_by_key(|x| match x {
            LuaLsType::Primitive(_) => 1,
            LuaLsType::Named(_, _) => 2,
            LuaLsType::Class { .. } => 0,
        });

        for t in types.iter().rev() {
            let t_fmt = format!("{t}");
            match t {
                LuaLsType::Primitive(_) => continue,
                LuaLsType::Named(n, _) => {
                    if t_fmt.eq(n) {
                        continue;
                    }

                    writeln!(w, "---@alias {n} {t_fmt}")?;
                }
                LuaLsType::Class { name, fields } => {
                    writeln!(w, "---@class {name}")?;
                    for (name, t) in fields {
                        let t_name = t.name();
                        writeln!(w, "---@field {name} {t_name}")?;
                    }
                }
            }
        }

        Ok(())
    }
}
