' Copyright (c) 2022 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 5.07.05

Const scancode.A = &h1C
Const scancode.B = &h32
Const scancode.C = &h21
Const scancode.D = &h23
Const scancode.E = &h24
Const scancode.F = &h2B
Const scancode.G = &h34
Const scancode.H = &h33
Const scancode.I = &h43
Const scancode.J = &h3B
Const scancode.K = &h42
Const scancode.L = &h4B
Const scancode.M = &h3A
Const scancode.N = &h31
Const scancode.O = &h44
Const scancode.P = &h4D
Const scancode.Q = &h15
Const scancode.R = &h2D
Const scancode.S = &h1B
Const scancode.T = &h2C
Const scancode.U = &h3C
Const scancode.V = &h2A
Const scancode.W = &h1D
Const scancode.X = &h22
Const scancode.Y = &h35
Const scancode.Z = &h1A

' Numbers
Const scancode.1 = &h16
Const scancode.2 = &h1E
Const scancode.3 = &h26
Const scancode.4 = &h25
Const scancode.5 = &h2E
Const scancode.6 = &h36
Const scancode.7 = &h3D
Const scancode.8 = &h3E
Const scancode.9 = &h46
Const scancode.0 = &h45

' Keypad
Const scancode.KEYPAD_1 = &h69
Const scancode.KEYPAD_2 = &h72
Const scancode.KEYPAD_3 = &h7A
Const scancode.KEYPAD_4 = &h6B
Const scancode.KEYPAD_5 = &h73
Const scancode.KEYPAD_6 = &h74
Const scancode.KEYPAD_7 = &h6C
Const scancode.KEYPAD_8 = &h75
Const scancode.KEYPAD_9 = &h7D
Const scancode.KEYPAD_0 = &h70
Const scancode.KEYPAD_ASTERISK = &h7C
Const scancode.KEYPAD_ENTER    = &hE05A
Const scancode.KEYPAD_HYPHEN   = &h7B
Const scancode.KEYPAD_PERIOD   = &h71
Const scancode.KEYPAD_PLUS     = &h79
Const scancode.KEYPAD_SLASH    = &hE04A

' Punctuation
Const scancode.SEMICOLON     = &h4C
Const scancode.SINGLE_QUOTE  = &h52
Const scancode.COMMA         = &h41
Const scancode.PERIOD        = &h49
Const scancode.SLASH         = &h4A
Const scancode.BACKTICK      = &h0E
Const scancode.SPACE         = &h29
Const scancode.HYPHEN        = &h4E
Const scancode.EQUALS        = &h55
Const scancode.SQUARE_BRA    = &h54
Const scancode.SQUARE_KET    = &h5B
Const scancode.HASH          = &h5D
Const scancode.BACKSLASH     = &h61

  ' Function keys
Const scancode.F1  = &h05
Const scancode.F2  = &h06
Const scancode.F3  = &h04
Const scancode.F4  = &h0C
Const scancode.F5  = &h03
Const scancode.F6  = &h0B
Const scancode.F7  = &h83
Const scancode.F8  = &h0A
Const scancode.F9  = &h01
Const scancode.F10 = &h09
Const scancode.F11 = &h78
Const scancode.F12 = &h07

' Special keys
Const scancode.ENTER        = &h5A
Const scancode.ESCAPE       = &h76
Const scancode.ALT_LEFT     = &h11
Const scancode.ALT_RIGHT    = &hE011
Const scancode.PRINT_SCREEN = &hE07C
Const scancode.INSERT       = &hE070
Const scancode.HOME         = &hE06C
Const scancode.PAGE_UP      = &hE07D
Const scancode.DELETE       = &hE071
Const scancode.BACKSPACE    = &h66
Const scancode.END          = &hE069
Const scancode.TAB          = &h0D
Const scancode.PAGE_DOWN    = &hE07A
Const scancode.UP           = &hE075
Const scancode.LEFT         = &hE06B
Const scancode.DOWN         = &hE072
Const scancode.RIGHT        = &hE074
Const scancode.SHIFT_LEFT   = &h12
Const scancode.SHIFT_RIGHT  = &h59
Const scancode.SCROLL_LOCK  = &h7E
Const scancode.CTRL_LEFT    = &h14
Const scancode.WIN_LEFT     = &hE01F
Const scancode.WIN_RIGHT    = &hE027
Const scancode.MENUS        = &hE02F
Const scancode.CTRL_RIGHT   = &hE014
Const scancode.NUM_LOCK     = &h77
Const scancode.CAPS_LOCK    = &h58
Const scancode.PAUSE_BREAK  = &hE11477E1F014E077
