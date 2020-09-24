# My Build of XMonad

I set my escape-key to super, so I think this config works best with that.
Here are the basic keybindings:
| modifiers    | key    | action                           |
| :----------: | :----: | :------------------------------: |
| Mod4         | Return | terminal                         |
| Mod4         | d      | dmenu_run                        |
| Mod4         | w      | qutebrowser                      |
| Mod4         | r      | ranger                           |
| Mod4         | Tab    | nextLayout                       |
| Mod4         | t      | tiling Layout                    |
| Mod4         | b      | hide bar (weird with gaps) |
| Mod4 + Shift | h      | increase number of masters       |
| Mod4 + Shift | l      | decrease number of masters       |
| Mod4 + Shift | Return | Swap focused with master         |
| Mod4         | j      | focus down                       |
| Mod4         | k      | focus up                         |
| Mod4         | h      | shrink master area               |
| Mod4         | l      | grow master area                 |
| Mod4 + Shift | c      | kill client                      |
| Mod4         | q      | restart xmonad                   |
| Mod4 + Shift | q      | exit xmonad                      |

Check xmonad.hs for more bindings like moving and resizing floating windows,
scratchpads or topicspaces.
I just used an xmobar configuration from the internet.
