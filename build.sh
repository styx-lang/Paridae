cd bootstrap
cargo run
cd ..
gcc -g -Wall -Wno-pointer-sign main.c entry.c -o main
