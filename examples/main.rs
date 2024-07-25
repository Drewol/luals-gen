use std::path::PathBuf;

use luals_gen::{LuaLsGen, ToLuaLsType};
use serde::Serialize;

#[derive(ToLuaLsType, Serialize, Default)]
#[serde(tag = "t")]
enum Options {
    #[default]
    Yes,
    No,
    Maybe {
        certainty: f32,
    },
    NamedVote {
        favor: u32,
        against: u32,
    },
}

#[derive(ToLuaLsType, Serialize, Default)]
#[serde(rename_all = "PascalCase")]
struct Bar {
    name: String,
    enabled: Box<bool>,
    value: f32,
    alpha_beta: [bool; 2],
    opt: Options,
}

#[derive(ToLuaLsType, Serialize, Default)]
#[serde(rename_all = "kebab-case")]
struct Foo {
    name: String,
    data_vec: Vec<u8>,
    #[serde(rename = "AABBcc")]
    option_a: Option<Bar>,
    path: PathBuf,
}

#[derive(ToLuaLsType, Default, Serialize)]
struct Baz(Foo, Bar);
fn main() {
    LuaLsGen::generate_types::<Baz>(std::io::stdout()).unwrap();
    println!("{}", serde_json::to_string_pretty(&Baz::default()).unwrap())
}
