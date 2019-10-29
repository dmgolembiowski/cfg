hs.window.animationDuration = 0

units = {
  left      = { x = 0.01, y = 0.01, w = 0.59, h = 0.98 },
  right     = { x = 0.61, y = 0.01, w = 0.38, h = 0.98 },
  right_top = { x = 0.61, y = 0.01, w = 0.38, h = 0.485 },
  right_bot = { x = 0.61, y = 0.51, w = 0.38, h = 0.485 },
  maximum   = { x = 0.01, y = 0.01, w = 0.98, h = 0.98 }
}


mash = { 'shift', 'ctrl', 'cmd' }

hs.hotkey.bind(mash, 'h', function() hs.window.focusedWindow():move(units.left,      nil, true) end)
hs.hotkey.bind(mash, 'l', function() hs.window.focusedWindow():move(units.right,     nil, true) end)
hs.hotkey.bind(mash, 'j', function() hs.window.focusedWindow():move(units.right_bot, nil, true) end)
hs.hotkey.bind(mash, 'k', function() hs.window.focusedWindow():move(units.right_top, nil, true) end)
hs.hotkey.bind(mash, 'm', function() hs.window.focusedWindow():move(units.maximum,   nil, true) end)
