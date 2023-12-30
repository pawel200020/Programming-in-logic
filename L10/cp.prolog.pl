copyFiles(File1, File2) :- open(File1, read, Stream1),
  open(File2, write, Stream2),
  get_char(Stream1, Char),
  process(Stream1, Stream2, Char),
  close(Stream1), close(Stream2).
   
process(Stream1, Stream2, end_of_file).
process(Stream1, Stream2, Char) :- put_char(Stream2, Char), get_char(Stream1, Char2), process(Stream1, Stream2, Char2).

divFile(File1, File2, File3) :- open(File1, read, Stream1), open(File2, write, Stream2), open(File3, write, Stream3),
    get_char(Stream1, Char),
    process2(Stream1, Stream2,Stream3, Char),
    close(Stream1), close(Stream2), close(Stream3).

process2(Stream1, Stream2, Stream3, end_of_file).
process2(Stream1, Stream2,Stream3, Char) :- put_char(Stream2, Char), get_char(Stream1, Char2), process2(Stream1, Stream3, Stream2, Char2).


mergeFile(File1, File2, File3):- open(File1, read, Stream1), open(File2, read, Stream2), open(File3, write, Stream3),
    get_char(Stream1, Char), get_char(Stream2, Char2),   
    insert(Stream1, Stream2, Stream3, Char, Char2),
    close(Stream1), close(Stream2), close(Stream3).

insert(Stream1, Stream2, Stream3, end_of_file, end_of_file).
insert(Stream1, Stream2, Stream3, Char, end_of_file):-put_char(Stream3, Char),get_char(Stream1, Char4), insert(Stream1, Stream2, Stream3, Char4, end_of_file).
insert(Stream1, Stream2, Stream3, end_of_file, Char):-put_char(Stream3, Char),get_char(Stream2, Char4), insert(Stream1, Stream2, Stream3, end_of_file, Char4).
insert(Stream1, Stream2, Stream3, Char, Char2):-put_char(Stream3, Char),put_char(Stream3, Char2),
    get_char(Stream1, Char3), get_char(Stream2, Char4), insert(Stream1, Stream2, Stream3, Char3, Char4).