# luals-gen

Generate [LuaLS](https://luals.github.io/) annotations for Rust structs.

Intended to be used together with [`mlua::LuaSerdeExt`](https://docs.rs/mlua/latest/mlua/trait.LuaSerdeExt.html#tymethod.to_value)

## Notes

- Serde rename attributes have basic support.
- Basically no testing has been done

## TODO

- Generate annotations based on [`mlua::UserData`](https://docs.rs/mlua/latest/mlua/trait.UserData.html)
- Enum support
- Better serde attribute support
- Alias other exported struct definitions

## Acknowledgements

Some inspiration from https://github.com/lenscas/tealr/
