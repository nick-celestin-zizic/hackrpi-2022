IF EXIST bin RMDIR /S /Q bin

cargo build --release --no-default-features --target wasm32-unknown-unknown

wasm-bindgen --out-name rover_game --out-dir bin --target web target/wasm32-unknown-unknown/release/hackrpi.wasm

mkdir bin/assets
xcopy assets "bin/assets\" /E

xcopy static bin /E
