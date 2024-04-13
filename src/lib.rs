use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
    fmt::{Display, Write},
    rc::Rc,
    sync::Arc,
};

pub use luals_gen_derive::ToLuaLsType;

#[derive(Debug)]
pub enum LuaLsType {
    Named(Cow<'static, str>),
    Union(Vec<LuaLsType>),
    Array(Box<LuaLsType>),
    Table {
        key: Box<LuaLsType>,
        value: Box<LuaLsType>,
    },
    TableLiteral(Vec<(Cow<'static, str>, LuaLsType)>),
}

pub trait ToLuaLsType {
    fn lua_ls_type() -> LuaLsType;
}

macro_rules! impl_ls_type {
    ($ls_type:literal $current_type:ty) => {
        impl ToLuaLsType for $current_type {
            fn lua_ls_type() -> LuaLsType {
                LuaLsType::Named($ls_type.into())
            }
        }
    };
    ($ls_type:literal $current_type:ty,$($types:ty),*) => {
        impl_ls_type!($ls_type $current_type);
        impl_ls_type!($ls_type $($types),+);
    };
}

impl_ls_type!("boolean" bool);
impl_ls_type!("string" String,std::ffi::CString,&str,&std::ffi::CStr);
impl_ls_type!("number" f32,f64);
impl_ls_type!("integer" i8,u8,u16,i16,u32,i32,u64,i64,u128,i128,isize,usize);

impl<T> ToLuaLsType for Vec<T>
where
    T: ToLuaLsType,
{
    fn lua_ls_type() -> LuaLsType {
        LuaLsType::Array(Box::new(T::lua_ls_type()))
    }
}

impl<T> ToLuaLsType for HashSet<T>
where
    T: ToLuaLsType,
{
    fn lua_ls_type() -> LuaLsType {
        LuaLsType::Array(Box::new(T::lua_ls_type()))
    }
}

impl<T> ToLuaLsType for Option<T>
where
    T: ToLuaLsType,
{
    fn lua_ls_type() -> LuaLsType {
        LuaLsType::Union(vec![T::lua_ls_type(), LuaLsType::Named("nil".into())])
    }
}

impl<K, V> ToLuaLsType for HashMap<K, V>
where
    K: ToLuaLsType,
    V: ToLuaLsType,
{
    fn lua_ls_type() -> LuaLsType {
        LuaLsType::Table {
            key: Box::new(K::lua_ls_type()),
            value: Box::new(V::lua_ls_type()),
        }
    }
}

impl<T, const N: usize> ToLuaLsType for [T; N]
where
    T: ToLuaLsType,
{
    fn lua_ls_type() -> LuaLsType {
        LuaLsType::TableLiteral(
            (1..=N)
                .map(|i| (format!("[{i}]").into(), T::lua_ls_type()))
                .collect(),
        )
    }
}

impl<T> ToLuaLsType for Box<T>
where
    T: ToLuaLsType,
{
    fn lua_ls_type() -> LuaLsType {
        T::lua_ls_type()
    }
}

impl<T> ToLuaLsType for Arc<T>
where
    T: ToLuaLsType,
{
    fn lua_ls_type() -> LuaLsType {
        T::lua_ls_type()
    }
}

impl<T> ToLuaLsType for Rc<T>
where
    T: ToLuaLsType,
{
    fn lua_ls_type() -> LuaLsType {
        T::lua_ls_type()
    }
}

impl Display for LuaLsType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LuaLsType::Named(n) => f.write_str(n)?,
            LuaLsType::Union(u) => {
                f.write_char('(')?;
                for (i, v) in u.iter().enumerate() {
                    if i != 0 {
                        f.write_str(" | ")?;
                    }

                    f.write_fmt(format_args!("{}", v))?;
                }
                f.write_char(')')?;
            }
            LuaLsType::Array(t) => f.write_fmt(format_args!("{}[]", t))?,
            LuaLsType::Table { key, value } => {
                f.write_fmt(format_args!("table<{}, {}>", key, value))?
            }
            LuaLsType::TableLiteral(lit) => {
                f.write_str("{ ")?;
                for (i, kv) in lit.iter().enumerate() {
                    f.write_fmt(format_args!("{}: {}", kv.0, kv.1))?;
                    if i < lit.len() - 1 {
                        f.write_str(", ")?;
                    }
                }
                f.write_str(" }")?;
            }
        }

        Ok(())
    }
}
