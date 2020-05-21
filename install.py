#/usr/bin/env python3
import os

home = os.path.expanduser("~")
if os.getcwd() != f"{home}/.dotfiles":
    raise Exception("The install script must be run from ~/.dotfiles.")

# ~/.config/*
for folder in os.listdir("config"):
    if os.path.isfile(folder):
        continue

    os.symlink(f"config/{folder}", f"{home}/.config/{folder}", True)
    print(f"Symlinked ~/.config/{folder}")

# Emacs
os.symlink("emacs.d", "{home}/.emacs.d", True)
print("Symlinked ~/.emacs.d")

# Antigen
os.symlink("zsh/antigen", "{home}/.antigen", True)
print("Symlinked ~/.antigen")

# Find symlinks
for root, dirs, filenames in os.walk("."):
    for filename in filenames:
        filesplit = os.path.splitext(filename)
        if filesplit[1] == ".symlink":
            os.symlink(f"{home}/.{filesplit[0]}")
            print(f"Symlinked ~/.{filesplit[0]}")
