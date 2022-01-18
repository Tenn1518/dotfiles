# qutebrowser configuration

config.load_autoconfig(False)
# Install python-adblock
c.content.blocking.method = "both"
c.zoom.default = "125%"
c.fonts.default_size = "12pt"
c.fonts.tabs.selected = "12pt Menlo LG S"
c.fonts.tabs.unselected = "12pt Menlo LG S"
c.fonts.statusbar = "12pt Menlo LG S"

c.url.searchengines['aw'] = 'https://wiki.archlinux.org/index.php?search={}'

# Keybindings
config.bind('[b', 'back')
config.bind(']b', 'forward')
