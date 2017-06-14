mod compiler;
mod link;

use std::env;
use std::fs::File;
use std::io::Write;

use llvm::Module;

use sem::ir::Program;

mod errors {
    error_chain! {
        errors {}
    }
}

pub use self::errors::Error;
use self::errors::ResultExt;

pub struct Emitter {
    module: Module,
}

impl Emitter {
    pub fn new(program: &Program) -> Result<Emitter, Error> {
        let mut c = compiler::Compiler::new();
        c.compile_program(program)
            .map_err(|err| Error::from(err.to_string()))?;
        Ok(Emitter { module: c.module })
    }

    pub fn emit_executable(&self, executable: &str) -> Result<(), Error> {
        let mut objpath = env::temp_dir();
        objpath.push(format!("{}.o", executable));
        let mut objfile = File::create(&objpath)
            .map_err(|err| Error::from(err.to_string()))
            .chain_err(|| {
                           format!("failed to create an object file: {}",
                                   objpath.to_string_lossy())
                       })?;
        let objbytes = self.module
            .emit_object()
            .map_err(|err| Error::from(err.to_string()))
            .chain_err(|| "failed to emit object code")?;
        objfile
            .write_all(&objbytes)
            .chain_err(|| "failed to write object code to a tmpfile")?;
        link::link(executable, &objpath.to_string_lossy())
    }

    pub fn emit_llvm_ir(&self) -> String {
        self.module.to_string()
    }
}
