# BCC

![GitHub watchers](https://img.shields.io/github/watchers/ianyourgod/batpu-c-compiler?style=for-the-badge&color=7137ef)
![GitHub Repo stars](https://img.shields.io/github/stars/ianyourgod/batpu-c-compiler?style=for-the-badge&color=efac37)
![GitHub forks](https://img.shields.io/github/forks/ianyourgod/batpu-c-compiler?style=for-the-badge)
![Static Badge](https://img.shields.io/badge/License-MIT-green?style=for-the-badge)

## The BatPU C Compiler

BCC is a C compiler for the BatPU architecture. It is based off of "Writing a C Compiler" by Nora Sandler, and is written in Rust.

### Table of Contents

- [BCC](#bcc)
  - [The BatPU C Compiler](#the-batpu-c-compiler)
    - [Table of Contents](#table-of-contents)
    - [Building](#building)
    - [Usage](#usage)
    - [License](#license)
    - [Contributing](#contributing)
    - [Feature Requests](#feature-requests)
    - [Acknowledgements](#acknowledgements)

### Building

To build BCC, you will need to have Rust installed. You can install Rust by following the instructions at [rustup.rs](https://rustup.rs/).

Once you have Rust installed, you can build BCC by running the following command:

```sh
cargo build --release
```

This will build the BCC compiler in release mode. The compiled binary will be located at `target/release/bcc`.  
You may also choose not to build. Compiling will be slightly slower, but not that much.

### Usage

To compile a C file when you've built, run the following command:

```sh
<compiler executible> <input_file> -o <output_file>
```

Compiler executible could be `target/release/bcc` if you built it yourself, or `c-compiler` if you're using the prebuilt binary.

To compile a C file without building, run the following command:

```sh
cargo run <input_file> -o <output_file>
```

These will compile the input file and output the resulting assembly to the output file.  
If you choose not to specify an output file, the assembly will be placed in `output.as`.

### License

BCC is licensed under the MIT License. See [LICENSE](LICENSE) for more information.

### Contributing

If you would like to contribute to BCC, please read [CONTRIBUTING.md](CONTRIBUTING.md) for more information.

### Feature Requests

If you would like to request a feature, please open an issue with the `feature-request` label.

### Acknowledgements

- [Writing a C Compiler](https://norasandler.com/2024/08/20/The-Book-Is-Here.html) by Nora Sandler
- [Rust](https://www.rust-lang.org/)
- [Me](https://github.com/ianyourgod)
