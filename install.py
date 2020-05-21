#/usr/bin/env python3
import os, subprocess

home = os.path.expanduser("~")
dotfiles = f"{home}/.dotfiles"
if os.getcwd() != f"{home}/.dotfiles":
    raise Exception("The install script must be run from ~/.dotfiles.")

# ~/.config/*
for folder in os.listdir("config"):
    if os.path.isfile(folder):
        continue

# Find symlinks
for root, dirs, filenames in os.walk("."):
    for filename in filenames:
        filesplit = os.path.splitext(filename)
        if filesplit[1] == ".symlink":
            os.symlink(f"{dotfiles}/{root}/{filename}", f"{home}/.{filesplit[0]}")
            print(f"Symlinked ~/.{filesplit[0]}")
    os.symlink(f"{dotfiles}/config/{folder}", f"{home}/.config/{folder}", True)
    print(f"Symlinked ~/.config/{folder}")

# Emacs
os.symlink(f"{dotfiles}/emacs.d", f"{home}/.emacs.d", True)
print("Symlinked ~/.emacs.d")

# Antigen
os.symlink(f"{dotfiles}/zsh/antigen", f"{home}/.antigen", True)
print("Symlinked ~/.antigen")

# Fonts
for folder in os.listdir(fonts):
    cmd = subprocess.call(f"{dotfiles}/fonts/{folder}/install.sh")
    print(f"Installed {folder} fonts.")
