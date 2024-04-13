use luals_gen::ToLuaLsType;
use serde::Serialize;
#[derive(ToLuaLsType, Serialize)]
#[serde(rename_all = "PascalCase")]
struct Bar {
    name: String,
    enabled: Box<bool>,
    value: f32,
    alpha_beta: [bool; 2],
}

#[derive(ToLuaLsType, Serialize)]
#[serde(rename_all = "kebab-case")]
struct Foo {
    name: String,
    data_vec: Vec<u8>,
    #[serde(rename = "AABBcc")]
    option_a: Option<Bar>,
}

#[derive(ToLuaLsType)]
struct Baz(Foo, Bar);
fn main() {
    println!("{}", Baz::lua_ls_type());
}
