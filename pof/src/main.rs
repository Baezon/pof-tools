// #![warn(missing_docs)]
#![allow(clippy::useless_format)]

mod parse;
mod types;
mod write;

pub use parse::parse_dae;
pub use parse::Parser;
pub use types::*;

use std::fs::File;
use std::io::{self, Write};

use walkdir::WalkDir;

fn main() -> io::Result<()> {
    // D:\\Freespace\\Knossos\\library\\FS2\\blueplanetcomplete-1.1.5\\data\\models\\avenger3.pof
    // has a bsp leaf with no polies?

    // for (i, entry) in WalkDir::new("D:\\Freespace\\Knossos\\library\\FS2").into_iter().enumerate().skip(500) {
    //     let entry = entry.unwrap();
    //     let filename: &str = entry.file_name().to_str().unwrap();
    //     if filename.ends_with(".pof") {
    //         println!("{}: Testing model {:#?}...", i, entry.path());
    //         let file = File::open(entry.path()).unwrap();
    //         let mut parser = Parser::new(file)?;
    //         let model = parser.parse(entry.path().to_path_buf())?;

    //         let mut file = File::create("output.pof").unwrap();
    //         model.write(&mut file).unwrap();
    //         let buf1 = std::fs::read("output.pof").unwrap();

    //         let file = File::open("output.pof").unwrap();
    //         let mut parser = Parser::new(file)?;
    //         let model = parser.parse(entry.path().to_path_buf())?;

    //         let mut file = File::create("output2.pof").unwrap();
    //         model.write(&mut file).unwrap();
    //         let buf2 = std::fs::read("output2.pof").unwrap();

    //         //assert!(buf1 == buf2);
    //         if buf1 != buf2 {
    //             println!("Buffers weren't equal!");
    //         }
    //     }
    // }

    // use std::path::PathBuf;
    // let path = PathBuf::from("D:\\Freespace\\Knossos\\library\\FS2\\blueplanetcomplete-1.1.5\\data\\models\\Cargo_Platform.pof");
    // let file = File::open(path.clone()).unwrap();
    // let mut parser = Parser::new(file)?;
    // let model = parser.parse(path.clone())?;

    // let mut file = File::create("output1.log").unwrap();
    // write!(file, "{:#?}", model);

    // let mut file = File::create("output.pof").unwrap();
    // model.write(&mut file).unwrap();
    // let buf1 = std::fs::read("output.pof").unwrap();

    // let file = File::open("output.pof").unwrap();
    // let mut parser = Parser::new(file)?;
    // let model = parser.parse(path)?;

    // let mut file = File::create("output2.log").unwrap();
    // write!(file, "{:#?}", model);

    // let mut file = File::create("output2.pof").unwrap();
    // model.write(&mut file).unwrap();
    // let buf2 = std::fs::read("output2.pof").unwrap();

    // //assert!(buf1 == buf2);
    // if buf1 != buf2 {
    //     println!("Buffers weren't equal!");
    // }

    Ok(())
}
