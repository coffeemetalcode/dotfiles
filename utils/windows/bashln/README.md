# `bashln`

## Summary

Invoke `bashln` from Windows terminal emulators (GitBash, Mintty, etc.) to create launchers for Windows `*.exe`, `*.py`, or plain (`bash`, `sh`, etc.) executable files in a `$PATH` directory for a more bash-like experience launching programs.

## Usage

`bashln` takes two arguments:
1. Location of executable file or files
2. Location of generated launcher script

```bash
bashln /c/Program\ Files/Git/bin/git.exe ~/.local/bin/
```

Creates a shell script (`~/.local/bin/git`) that handles invoking `/c/Program Files/Git/bin/git.exe` from terminal emulators like Mintty, CygWin, GitBash, etc.

```bash
bashln ~/Apps/lilypond-2.26.0/bin/* ~/.local/bin/
```

Creates scripts for all `*.exe`, `*.py`, for plain (`bash`, `sh`, etc.) executables in the target directory

## Prompt

In the file `utils/windows/bashln`, write a bash script that creates program launchers invocable from Windows-based or WSL terminal emulators (like WSL, Mintty, GitBash, CygWin, etc.).

**Requirements**:
1. Takes a single file or directory of files a first argument
2. Takes a single filename or directory as a second argument
3. Constraint: If the first argument is a filename, it must have an extension of `*.exe`, `*.py`, or no extension, or else the script will exit with an error message
4. Constraint: If the first argument is a directory, it will only create launchers for `*.exe`, `*.py`, or executable files with no extension. If no such files are found, the script will exit with a message. If a plain (no extension) file is not executable, a launcher for it will not be created
5. When creating launchers in the target directory or filename:
  1. Take the name of the source file
  2. If the source file has an extension, drop the extension for the name of the launcher (e.g. `/source/program.exe` becomes `/destination/program`)
  3. If the source argument is a directory, the destination argument must also be a directory -- if it is not, then exit and warn the user
  4. Allow for when the user wants to specify the destination filename (e.g. `bashln /source/program.py destination/program2`)
6. Make all created destination launcher files executable with `chmod +x <generated-launcher-name>`
7. Use the file `utils/windows/example` for an example of the contents of the destination launcher
  1. Note that if the source directory has a space, the `exec` line drops the escape character from the path, since this is not needed due to the path being enclosed in `""`