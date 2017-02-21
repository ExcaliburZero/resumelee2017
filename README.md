# Résumélee 2017
This is my entry for BSides RoC's 2017 Résumélee competition.

## Usage
First, make sure that you have [Stack](https://docs.haskellstack.org/en/stable/README/) installed. Then run the following command in the main directory of the project to get the correct compiler version and compile the program.

```
$ stack setup
$ stack build
$ stack install
```

## Encrypting Files
To encrypt a file, run the following command, specifying the file to encrypt and number of pieces to split it into.

```
$ stack exec resumelee2017 -- encrypt FILE_NAME NUM_PIECES
```

For example, you can encrypt a file `resume.pdf` into 5 pieces by doing the following:

```
$ ls
resume.pdf

$ stack exec resumelee2017 -- encrypt resume.pdf 5
Encrypting file: resume.pdf

$ ls
resume.pdf        resume.pdf.part2  resume.pdf.part4
resume.pdf.part1  resume.pdf.part3  resume.pdf.part5
```

## Decrypting Files
To decrypt a file, place all of the file pieces in the same directory and give the name of the decrypted file and the number of file pieces.

```
$ stack exec resumelee2017 -- decrypt FILE_NAME NUM_PIECES
```

For example, you can decrypt a file `resume.pdf` that is split into 5 pieces by doing the following:

```
$ ls
resume.pdf.part1  resume.pdf.part3  resume.pdf.part5
resume.pdf.part2  resume.pdf.part4

$ stack exec resumelee2017 -- decrypt resume.pdf 5
Decrypting file: resume.pdf

$ ls
resume.pdf        resume.pdf.part2  resume.pdf.part4
resume.pdf.part1  resume.pdf.part3  resume.pdf.part5
```

## License
The source code of resumelee2017 is available under the [MIT license](https://opensource.org/licenses/MIT), see `LICENSE` for more information.
