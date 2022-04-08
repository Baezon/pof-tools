pub mod vp;

use pof::{Model, Parser};
use std::{fs::File, io, path::Path};

fn process_path(path: &Path, f: &mut impl FnMut(&Path, Model)) -> io::Result<()> {
    match path.extension().and_then(|s| s.to_str()) {
        Some("pof" | "POF") => {
            let file = File::open(path)?;
            println!("- parsing {}...", path.display());
            f(path, Parser::new(file)?.parse(path.to_owned())?);
        }
        Some("vp" | "VP") => {
            let mut vp = vp::VP::new(path)?;
            let mut iter = vp.files();
            while let Some(mut file) = iter.next_file() {
                if file.name.ends_with(".pof") || file.name.ends_with(".POF") {
                    let mut path2 = path.to_owned();
                    path2.extend(file.path);
                    path2.push(file.name);
                    let file = file.reader()?;
                    println!("- parsing {}...", path2.display());
                    f(&path2, Parser::new(file)?.parse(path.to_owned())?);
                }
            }
        }
        _ => {}
    }
    Ok(())
}

fn run_census(locations: impl IntoIterator<Item = String>, mut f: impl FnMut(&Path, Model)) {
    for loc in locations {
        for entry in walkdir::WalkDir::new(loc) {
            let entry = entry.unwrap();
            if entry.file_type().is_file() {
                drop(process_path(entry.path(), &mut f));
            }
        }
    }
}

fn main() {
    run_census(std::env::args().skip(1), |_path, model| {
        // TODO: insert interesting POF question here
        println!("{} subobjects", model.sub_objects.len())
    });
}
