use std::fs::File;
use std::io::{self, Read, Seek, SeekFrom};
use std::path::Path;
use std::time::{Duration, SystemTime};

use byteorder::{ReadBytesExt, LE};

pub enum DirEntry {
    Backdir,
    Directory { name: String, timestamp: u32 },
    File { offset: u32, size: u32, name: String, timestamp: u32 },
}

pub struct VP {
    file: File,
    entries: Vec<DirEntry>,
}

impl VP {
    pub fn entries(file: &mut (impl Read + Seek)) -> io::Result<Vec<DirEntry>> {
        let mut buf = [0; 4];
        file.read_exact(&mut buf)?;
        if buf != *b"VPVP" {
            return Err(io::Error::new(io::ErrorKind::InvalidData, "not a VP file"));
        }
        if file.read_u32::<LE>()? != 2 {
            return Err(io::Error::new(io::ErrorKind::InvalidData, "unknown VP version"));
        }
        let offset = file.read_u32::<LE>()?;
        let num_entries = file.read_u32::<LE>()?;
        file.seek(SeekFrom::Start(offset.into()))?;
        let mut buf = [0; 32];
        let mut entries = Vec::with_capacity(num_entries as usize);
        for _ in 0..num_entries {
            let offset = file.read_u32::<LE>()?;
            let size = file.read_u32::<LE>()?;
            file.read_exact(&mut buf)?;
            let name = std::str::from_utf8(&buf[..memchr::memchr(0, &buf).unwrap_or(buf.len())])
                .map_err(|_| io::Error::new(io::ErrorKind::InvalidData, "non UTF-8"))?;
            let timestamp = file.read_u32::<LE>()?;
            entries.push(if size == 0 && timestamp == 0 {
                if name == ".." {
                    DirEntry::Backdir
                } else {
                    DirEntry::Directory { name: name.to_owned(), timestamp }
                }
            } else {
                DirEntry::File { offset, size, name: name.to_owned(), timestamp }
            })
        }
        Ok(entries)
    }

    pub fn new(path: impl AsRef<Path>) -> io::Result<Self> {
        let mut file = File::open(path)?;
        Ok(Self { entries: VP::entries(&mut file)?, file })
    }

    pub fn files(&mut self) -> FileIter<'_> {
        FileIter {
            file: &mut self.file,
            stack: vec![],
            entries: self.entries.iter(),
        }
    }
}

pub struct FileIter<'a> {
    file: &'a mut File,
    stack: Vec<&'a str>,
    entries: std::slice::Iter<'a, DirEntry>,
}

impl<'a> FileIter<'a> {
    pub fn next_file(&mut self) -> Option<FileEntry<'a, '_>> {
        loop {
            match self.entries.next()? {
                DirEntry::Backdir => {
                    self.stack.pop();
                }
                DirEntry::Directory { name, .. } => {
                    self.stack.push(name);
                }
                &DirEntry::File { offset, size, ref name, timestamp } => {
                    return Some(FileEntry {
                        file: self.file,
                        path: &self.stack,
                        offset,
                        size,
                        name,
                        timestamp,
                    })
                }
            }
        }
    }
}

pub struct FileEntry<'a, 'b> {
    file: &'b mut File,
    pub path: &'b [&'a str],
    offset: u32,
    size: u32,
    pub name: &'a str,
    timestamp: u32,
}

impl<'a, 'b> FileEntry<'a, 'b> {
    pub fn reader(&mut self) -> io::Result<FileSlice<'_>> {
        FileSlice::new(self.file, self.offset.into(), self.size.into())
    }

    pub fn as_slice(&mut self) -> io::Result<Box<[u8]>> {
        let mut buf = vec![0; self.size as usize].into_boxed_slice();
        self.file.seek(SeekFrom::Start(self.offset.into()))?;
        self.file.read_exact(&mut buf)?;
        Ok(buf)
    }

    pub fn modified(&self) -> SystemTime {
        SystemTime::UNIX_EPOCH + Duration::from_secs(self.timestamp.into())
    }
}

pub struct FileSlice<'a> {
    file: &'a mut File,
    start_offset: u64,
    len: u64,
    remaining: u64,
}

impl<'a> FileSlice<'a> {
    fn new(file: &'a mut File, start: u64, len: u64) -> io::Result<Self> {
        file.seek(SeekFrom::Start(start))?;
        Ok(Self { file, start_offset: start, len, remaining: len })
    }
}

impl Read for FileSlice<'_> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let len = buf.len().min(self.remaining as usize);
        let count = self.file.read(&mut buf[..len])?;
        self.remaining -= count as u64;
        Ok(count)
    }
}

impl Seek for FileSlice<'_> {
    fn seek(&mut self, pos: SeekFrom) -> io::Result<u64> {
        let out = self.file.seek(match pos {
            SeekFrom::Start(n) => SeekFrom::Start(n.min(self.len) + self.start_offset),
            SeekFrom::End(n) => SeekFrom::Start(self.start_offset + ((self.len as i64) + n.max(0)) as u64),
            SeekFrom::Current(n) => SeekFrom::Current(n.min(self.remaining as i64)),
        })? - self.start_offset;
        self.remaining = self.len - out;
        Ok(out)
    }
}
