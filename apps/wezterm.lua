-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This table will hold the configuration.
local config = {}

-- In newer versions of wezterm, use the config_builder which will
-- help provide clearer error messages
if wezterm.config_builder then
  config = wezterm.config_builder()
end

-- This is where you actually apply your config choices
-- TODO: Find a better theme or create my own
config.color_scheme = 'Panda (Gogh)'

-- Window opacity
config.window_background_opacity = 0.96

-- and finally, return the configuration to wezterm
return config

