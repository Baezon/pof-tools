// #![warn(missing_docs)]

mod parse;
mod types;
mod write;

pub use parse::parse_dae;
pub use parse::Parser;
pub use types::*;

use std::fs::File;
use std::io;

use walkdir::WalkDir;

fn main() -> io::Result<()> {
    // D:\\Freespace\\Knossos\\library\\FS2\\blueplanetcomplete-1.1.5\\data\\models\\avenger3.pof
    // has a bsp leaf with no polies?

    // for (i, entry) in WalkDir::new("D:\\Freespace\\Knossos\\library\\FS2")
    //     .into_iter()
    //     .enumerate()
    //     .skip(500)
    // {
    //     let entry = entry.unwrap();
    //     let filename: &str = entry.file_name().to_str().unwrap();
    //     if filename.ends_with(".pof") {
    //         println!("{}: Testing model {:#?}...", i, entry.path());
    //         let file = File::open(entry.path()).unwrap();
    //         let mut parser = Parser::new(file)?;
    //         let model = parser.parse()?;

    //         let mut file = File::create("output.pof").unwrap();
    //         model.write(&mut file).unwrap();
    //         let buf1 = std::fs::read("output.pof").unwrap();

    //         let file = File::open("output.pof").unwrap();
    //         let mut parser = Parser::new(file)?;
    //         let model = parser.parse()?;

    //         let mut file = File::create("output2.pof").unwrap();
    //         model.write(&mut file).unwrap();
    //         let buf2 = std::fs::read("output2.pof").unwrap();

    //         //assert!(buf1 == buf2);
    //         if buf1 != buf2 {
    //             println!("Buffers weren't equal!");
    //         }
    //     }
    // }

    // let file = File::open("EMPulse.pof").unwrap();
    // let mut parser = Parser::new(file)?;
    // let model = parser.parse()?;
    // for (bbox, list) in model.sub_objects[0].bsp_data.collision_tree.leaves() {
    //     println!("{:#?}",bbox);
    // }

    // let mut file = File::create("output.pof").unwrap();+

    // write_pof(&mut file, &model, Version::V21_17).unwrap();
    // write!(File::create("output.log").unwrap(), "{:#?}", model).unwrap();
    // let buf1 = std::fs::read("output.pof").unwrap();

    // let file = File::open("output.pof").unwrap();
    // let mut parser = Parser::new(file)?;
    // let model = parser.parse()?;
    // write!(File::create("output2.log").unwrap(), "{:#?}", model).unwrap();

    // let mut file = File::create("output2.pof").unwrap();
    // write_pof(&mut file, &model, Version::V21_17).unwrap();
    // let buf2 = std::fs::read("output2.pof").unwrap();

    // assert!(buf1 == buf2);

    parse::parse_dae("TEST.dae");

    Ok(())
}
