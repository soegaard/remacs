#lang racket
(require (for-syntax syntax/parse racket/syntax))
(require ffi/unsafe/objc)
(require ffi/unsafe
         ffi/unsafe/objc
         ffi/unsafe/define
         mred/private/wx/cocoa/image
         mred/private/wx/cocoa/types)

;;; Bit operations
(define (<< x y) (arithmetic-shift x y))
(define (>> x y) (arithmetic-shift x (- y)))

;;; Libraries used
(define quartz-lib (ffi-lib "/System/Library/Frameworks/Quartz.framework/Versions/Current/Quartz"))
(define carbon-lib (ffi-lib "/System/Library/Frameworks/Carbon.framework/Versions/Current/Carbon"))
(define carbon-core-lib 
  (ffi-lib (string-append "/System/Library/Frameworks/CoreServices.framework/"
                          "Frameworks/CarbonCore.framework/Versions/Current/CarbonCore")))
(define cf-lib 
  (case (system-type)
    [(macosx) (ffi-lib "/System/Library/Frameworks/CoreFoundation.framework/CoreFoundation")]
    [else #f]))

(define-ffi-definer define-quartz      quartz-lib)
(define-ffi-definer define-carbon-core carbon-core-lib)
(define-ffi-definer define-carbon      carbon-lib)
(define-ffi-definer define-cf          cf-lib #:default-make-fail make-not-available)


;;; API CONSTANTS
(define-carbon kTISPropertyUnicodeKeyLayoutData _NSString)

;;; CORE FOUNDATION

(import-class NSString)
(define _CFStringRef _NSString)

; (define _OSStatus _sint32) ; already imported

(define-cpointer-type _CFDataRef)
(define-cf CFDataGetBytePtr
  (_fun _CFDataRef -> _pointer))

;;; Types from MacTypes.h
(define _UniChar              _uint16)
(define _UniCharCount         _ulong)
(define _UniCharPointer      (_ptr io _UniChar))
(define _UniCharCountPointer (_ptr io _UniCharCount))
(define _OptionBits           _uint32)

;;; TEXT INPUT SOURCES

;;; Get the current text input source (read: the keyboard)
(define _TISInputSourceRef (_cpointer 'TISInputSourceRef))
(define-carbon TISCopyCurrentKeyboardLayoutInputSource (_fun -> _TISInputSourceRef))

;;; Used to get the layout of the keyboard from an text input source
(define-carbon TISGetInputSourceProperty
  (_fun (_inputSource : _TISInputSourceRef)
        (_propertyKey : _CFStringRef)           
        -> (_or-null _CFDataRef)))

(define-carbon LMGetKbdType (_fun -> _uint8))

(define _UCKeyboardLayout (_cpointer 'UCKeyboardLayout))

(define-carbon UCKeyTranslate
  (_fun (keyboardLayoutPtr   : _UCKeyboardLayout)
        (virtualKeyCode      : _uint16)
        (keyAction           : _uint16)
        (modifierKeyState    : _uint32)
        (keyboardType        : _uint32)
        (keyTranslateOptions : _OptionBits)            ; uint32
        ; (deadKeyState        : (_ptr i _uint32))
        (deadKeyState        : (_box _uint32))
        (maxStringLength     : _UniCharCount)
        ; (actualStringLength  : _UniCharCountPointer)
        (actualStringLength  : (_box _UniCharCount))
        (unicodeString       : _pointer)
        -> _OSStatus))

;;;
;;; EventModifiers (UInt16)
;;;

; From (file dates 2008):
; /System/Library/Frameworks/Carbon.framework/Versions/A/
; Frameworks/HIToolbox.framework/Versions/A/Headers/Events.h 

; The definitions indicate which bit controls what.
(define modifier-active-flag-bit     0) ; activeFlagBit = 0,  /* activate?(activateEvt and mouseDown)
(define modifier-btn-state-bit       7) ; btnStateBit  = 7,   /* state of button?*/
(define modifier-cmd-key-bit         8) ;                     /* command key down?*/
(define modifier-shift-key-bit       9) ; shiftKeyBit   = 9,  /* shift key down?*/
(define modifier-alpha-lock-bit     10) ; alphaLockBit  = 10, /* alpha lock down?*/
(define modifier-option-bit         11) ; optionKeyBit  = 11, /* option key down?*/
(define modifier-control-key-bit    12) ; controlKeyBit = 12, /* control key down?*/
; NOTE: The following 3 modifiers are not supported on OS X
(define modifier-right-shift-key-bit   13) ; rightShiftKeyBit   = 13, /* right shift key down? */
(define modifier-right-option-key-bit  14) ; rightOptionKeyBit  = 14, /* right Option key down? */
(define modifier-right-control-key-bit 15) ; rightControlKeyBit = 15  /* right Control key down? */


(define modifier-active-flag       (<< 1  0))
(define modifier-btn-state         (<< 1  7))
(define modifier-cmd-key           (<< 1  8))
(define modifier-shift-key         (<< 1  9))
(define modifier-alpha-lock        (<< 1 10)) 
(define modifier-option-key        (<< 1 11)) 
(define modifier-control-key       (<< 1 12)) 
; NOTE: The following 3 modifiers are not supported on OS X
(define modifier-right-shift-key   (<< 1 13))
(define modifier-right-option-key  (<< 1 14))
(define modifier-right-control-key (<< 1 15))


;;;
;;; MacRoman character codes
;;;
(define-syntax (define-name/value-definer stx)
  (syntax-parse stx
    [(_ prefix)
     (with-syntax ([prefix-name-to-value-ht (format-id stx "~a-name-to-value-ht" #'prefix)]
                   [prefix-value-to-name-ht (format-id stx "~a-value-to-name-ht" #'prefix)]
                   [prefix-name             (format-id stx "~a-name"             #'prefix)]
                   [prefix-value            (format-id stx "~a-value"            #'prefix)]
                   [define-prefix           (format-id stx "define-~a"           #'prefix)])
       #'(begin
           (define prefix-name-to-value-ht (make-hash))
           (define prefix-value-to-name-ht (make-hash))
           (define (prefix-name value) (hash-ref prefix-value-to-name-ht value #f))
           (define (prefix-value name) (hash-ref prefix-name-to-value-ht name #f))
           (provide prefix-name-to-value-ht 
                    prefix-value-to-name-ht
                    prefix-name
                    prefix-value)
           (define-syntax (define-prefix stx)
             (syntax-parse stx
               [(_ name expr)
                #'(begin
                    (provide name )
                    (define name expr)
                    (hash-set! prefix-name-to-value-ht 'name name)
                    (hash-set! prefix-value-to-name-ht name 'name))]))))]))

(define-name/value-definer mac-roman)

(define-mac-roman kNullCharCode                0)
(define-mac-roman kHomeCharCode                1)
(define-mac-roman kEnterCharCode               3)
(define-mac-roman kEndCharCode                 4)
(define-mac-roman kHelpCharCode                5)
(define-mac-roman kBellCharCode                7)
(define-mac-roman kBackspaceCharCode           8)
(define-mac-roman kTabCharCode                 9)
(define-mac-roman kLineFeedCharCode            10)
(define-mac-roman kVerticalTabCharCode         11)
(define-mac-roman kPageUpCharCode              11)
(define-mac-roman kFormFeedCharCode            12)
(define-mac-roman kPageDownCharCode            12)
(define-mac-roman kReturnCharCode              13)
(define-mac-roman kFunctionKeyCharCode         16)
(define-mac-roman kCommandCharCode             17)  ; /* glyph available only in system fonts*/
(define-mac-roman kCheckCharCode               18)  ; /* glyph available only in system fonts*/
(define-mac-roman kDiamondCharCode             19)  ; /* glyph available only in system fonts*/
(define-mac-roman kAppleLogoCharCode           20)  ; /* glyph available only in system fonts*/
(define-mac-roman kEscapeCharCode              27)
(define-mac-roman kClearCharCode               27)
(define-mac-roman kLeftArrowCharCode           28)
(define-mac-roman kRightArrowCharCode          29)
(define-mac-roman kUpArrowCharCode             30)
(define-mac-roman kDownArrowCharCode           31)
(define-mac-roman kSpaceCharCode               32)
(define-mac-roman kDeleteCharCode              127)
(define-mac-roman kBulletCharCode              165)
(define-mac-roman kNonBreakingSpaceCharCode    202)

;;;
;;; Useful Unicode key points
;;;

(define-name/value-definer unicode-key)

(define-unicode-key kShiftUnicode      #x21E7) ;/* Unicode UPWARDS WHITE ARROW*/
(define-unicode-key kControlUnicode    #x2303) ;/* Unicode UP ARROWHEAD*/
(define-unicode-key kOptionUnicode     #x2325) ;/* Unicode OPTION KEY*/
(define-unicode-key kCommandUnicode    #x2318) ;/* Unicode PLACE OF INTEREST SIGN*/
(define-unicode-key kPencilUnicode     #x270E) ;/* Unicode LOWER RIGHT PENCIL; 
;                                                         actually pointed left until Mac OS X 10.3*/
(define-unicode-key kPencilLeftUnicode #xF802) ;/* Unicode LOWER LEFT PENCIL; 
;                                                         available in Mac OS X 10.3 and later*/
(define-unicode-key kCheckUnicode      #x2713) ;/* Unicode CHECK MARK*/
(define-unicode-key kDiamondUnicode    #x25C6) ;/* Unicode BLACK DIAMOND*/
(define-unicode-key kBulletUnicode     #x2022) ;/* Unicode BULLET*/
(define-unicode-key kAppleLogoUnicode  #xF8FF) ;/* Unicode APPLE LOGO*/

;/*
; *  Summary:
; *    Virtual keycodes
; *  
; *  Discussion:
; *    These constants are the virtual keycodes defined originally in
; *    Inside Mac Volume V, pg. V-191. They identify physical keys on a
; *    keyboard. Those constants with "ANSI" in the name are labeled
; *    according to the key position on an ANSI-standard US keyboard.
; *    For example, kVK_ANSI_A indicates the virtual keycode for the key
; *    with the letter 'A' in the US keyboard layout. Other keyboard
; *    layouts may have the 'A' key label on a different physical key;
; *    in this case, pressing 'A' will generate a different virtual
; *    keycode.
; */
(define-name/value-definer virtual-key-code)

(define-virtual-key-code kVK_ANSI_A                   #x00)
(define-virtual-key-code kVK_ANSI_S                   #x01)
(define-virtual-key-code kVK_ANSI_D                   #x02)
(define-virtual-key-code kVK_ANSI_F                   #x03)
(define-virtual-key-code kVK_ANSI_H                   #x04)
(define-virtual-key-code kVK_ANSI_G                   #x05)
(define-virtual-key-code kVK_ANSI_Z                   #x06)
(define-virtual-key-code kVK_ANSI_X                   #x07)
(define-virtual-key-code kVK_ANSI_C                   #x08)
(define-virtual-key-code kVK_ANSI_V                   #x09)
(define-virtual-key-code kVK_ANSI_B                   #x0B)
(define-virtual-key-code kVK_ANSI_Q                   #x0C)
(define-virtual-key-code kVK_ANSI_W                   #x0D)
(define-virtual-key-code kVK_ANSI_E                   #x0E)
(define-virtual-key-code kVK_ANSI_R                   #x0F)
(define-virtual-key-code kVK_ANSI_Y                   #x10)
(define-virtual-key-code kVK_ANSI_T                   #x11)
(define-virtual-key-code kVK_ANSI_1                   #x12)
(define-virtual-key-code kVK_ANSI_2                   #x13)
(define-virtual-key-code kVK_ANSI_3                   #x14)
(define-virtual-key-code kVK_ANSI_4                   #x15)
(define-virtual-key-code kVK_ANSI_6                   #x16)
(define-virtual-key-code kVK_ANSI_5                   #x17)
(define-virtual-key-code kVK_ANSI_Equal               #x18)
(define-virtual-key-code kVK_ANSI_9                   #x19)
(define-virtual-key-code kVK_ANSI_7                   #x1A)
(define-virtual-key-code kVK_ANSI_Minus               #x1B)
(define-virtual-key-code kVK_ANSI_8                   #x1C)
(define-virtual-key-code kVK_ANSI_0                   #x1D)
(define-virtual-key-code kVK_ANSI_RightBracket        #x1E)
(define-virtual-key-code kVK_ANSI_O                   #x1F)
(define-virtual-key-code kVK_ANSI_U                   #x20)
(define-virtual-key-code kVK_ANSI_LeftBracket         #x21)
(define-virtual-key-code kVK_ANSI_I                   #x22)
(define-virtual-key-code kVK_ANSI_P                   #x23)
(define-virtual-key-code kVK_ANSI_L                   #x25)
(define-virtual-key-code kVK_ANSI_J                   #x26)
(define-virtual-key-code kVK_ANSI_Quote               #x27)
(define-virtual-key-code kVK_ANSI_K                   #x28)
(define-virtual-key-code kVK_ANSI_Semicolon           #x29)
(define-virtual-key-code kVK_ANSI_Backslash           #x2A)
(define-virtual-key-code kVK_ANSI_Comma               #x2B)
(define-virtual-key-code kVK_ANSI_Slash               #x2C)
(define-virtual-key-code kVK_ANSI_N                   #x2D)
(define-virtual-key-code kVK_ANSI_M                   #x2E)
(define-virtual-key-code kVK_ANSI_Period              #x2F)
(define-virtual-key-code kVK_ANSI_Grave               #x32)
(define-virtual-key-code kVK_ANSI_KeypadDecimal       #x41)
(define-virtual-key-code kVK_ANSI_KeypadMultiply      #x43)
(define-virtual-key-code kVK_ANSI_KeypadPlus          #x45)
(define-virtual-key-code kVK_ANSI_KeypadClear         #x47)
(define-virtual-key-code kVK_ANSI_KeypadDivide        #x4B)
(define-virtual-key-code kVK_ANSI_KeypadEnter         #x4C)
(define-virtual-key-code kVK_ANSI_KeypadMinus         #x4E)
(define-virtual-key-code kVK_ANSI_KeypadEquals        #x51)
(define-virtual-key-code kVK_ANSI_Keypad0             #x52)
(define-virtual-key-code kVK_ANSI_Keypad1             #x53)
(define-virtual-key-code kVK_ANSI_Keypad2             #x54)
(define-virtual-key-code kVK_ANSI_Keypad3             #x55)
(define-virtual-key-code kVK_ANSI_Keypad4             #x56)
(define-virtual-key-code kVK_ANSI_Keypad5             #x57)
(define-virtual-key-code kVK_ANSI_Keypad6             #x58)
(define-virtual-key-code kVK_ANSI_Keypad7             #x59)
(define-virtual-key-code kVK_ANSI_Keypad8             #x5B)
(define-virtual-key-code kVK_ANSI_Keypad9             #x5C)

; /* keycodes for keys that are independent of keyboard layout*/
(define-virtual-key-code kVK_Return                   #x24)
(define-virtual-key-code kVK_Tab                      #x30)
(define-virtual-key-code kVK_Space                    #x31)
(define-virtual-key-code kVK_Delete                   #x33)
(define-virtual-key-code kVK_Escape                   #x35)
(define-virtual-key-code kVK_Command                  #x37)
(define-virtual-key-code kVK_Shift                    #x38)
(define-virtual-key-code kVK_CapsLock                 #x39)
(define-virtual-key-code kVK_Option                   #x3A)
(define-virtual-key-code kVK_Control                  #x3B)
(define-virtual-key-code kVK_RightShift               #x3C)
(define-virtual-key-code kVK_RightOption              #x3D)
(define-virtual-key-code kVK_RightControl             #x3E)
(define-virtual-key-code kVK_Function                 #x3F)
(define-virtual-key-code kVK_F17                      #x40)
(define-virtual-key-code kVK_VolumeUp                 #x48)
(define-virtual-key-code kVK_VolumeDown               #x49)
(define-virtual-key-code kVK_Mute                     #x4A)
(define-virtual-key-code kVK_F18                      #x4F)
(define-virtual-key-code kVK_F19                      #x50)
(define-virtual-key-code kVK_F20                      #x5A)
(define-virtual-key-code kVK_F5                       #x60)
(define-virtual-key-code kVK_F6                       #x61)
(define-virtual-key-code kVK_F7                       #x62)
(define-virtual-key-code kVK_F3                       #x63)
(define-virtual-key-code kVK_F8                       #x64)
(define-virtual-key-code kVK_F9                       #x65)
(define-virtual-key-code kVK_F11                      #x67)
(define-virtual-key-code kVK_F13                      #x69)
(define-virtual-key-code kVK_F16                      #x6A)
(define-virtual-key-code kVK_F14                      #x6B)
(define-virtual-key-code kVK_F10                      #x6D)
(define-virtual-key-code kVK_F12                      #x6F)
(define-virtual-key-code kVK_F15                      #x71)
(define-virtual-key-code kVK_Help                     #x72)
(define-virtual-key-code kVK_Home                     #x73)
(define-virtual-key-code kVK_PageUp                   #x74)
(define-virtual-key-code kVK_ForwardDelete            #x75)
(define-virtual-key-code kVK_F4                       #x76)
(define-virtual-key-code kVK_End                      #x77)
(define-virtual-key-code kVK_F2                       #x78)
(define-virtual-key-code kVK_PageDown                 #x79)
(define-virtual-key-code kVK_F1                       #x7A)
(define-virtual-key-code kVK_LeftArrow                #x7B)
(define-virtual-key-code kVK_RightArrow               #x7C)
(define-virtual-key-code kVK_DownArrow                #x7D)
(define-virtual-key-code kVK_UpArrow                  #x7E)

; /* ISO keyboards only*/
(define-virtual-key-code kVK_ISO_Section              #x0A)

; /* JIS keyboards only*/
(define-virtual-key-code kVK_JIS_Yen                  #x5D)
(define-virtual-key-code kVK_JIS_Underscore           #x5E)
(define-virtual-key-code kVK_JIS_KeypadComma          #x5F)
(define-virtual-key-code kVK_JIS_Eisu                 #x66)
(define-virtual-key-code kVK_JIS_Kana                 #x68)

;;;
;;; Key Actions
;;;

(define-name/value-definer key-action)
(define-key-action kUCKeyActionDown    0) ; /* key is going down*/
(define-key-action kUCKeyActionUp      1) ; /* key is going up*/
(define-key-action kUCKeyActionAutoKey 2) ; /* auto-key down*/
(define-key-action kUCKeyActionDisplay 3) ; /* get information for key display (as in Key Caps) */

;;;
;;; Key Translate Options
;;;
; only one option ...
(define kUCKeyTranslateNoDeadKeysBit  0) ; /* Prevents setting any new dead-key states*/
(define kUCKeyTranslateNoDeadKeysFlag 1)
(define kUCKeyTranslateNoDeadKeysMask 1)

;;;
;;; Racket interface to UCKeyTranslate
;;;



(define cached-keyboard-layout #f)

(define (get-current-keyboard-layout)
  (define keyboard     (TISCopyCurrentKeyboardLayoutInputSource))
  (define layout-data  (TISGetInputSourceProperty keyboard kTISPropertyUnicodeKeyLayoutData))
  (define layout       (CFDataGetBytePtr layout-data))
  (cpointer-push-tag! layout 'UCKeyboardLayout) ; cast to UCKeyboardLayout
  layout)

; Note: 
;     dead-key-state 
;       A pointer to an unsigned 32-bit value, initialized to zero. 
;       The UCKeyTranslate function uses this value to store private 
;       information about the current dead key state.
;       (deadKeyState : (_ptr i _uint32))

(define max-string-length 255) 
(define output-chars (malloc _UniChar max-string-length)) ; allocate once only

(define (key-translate virtual-key-code 
                       #:key-action            [key-action         kUCKeyActionDown]
                       #:modifier-key-state    [modifier-key-state                0]  ; no modifier
                       #:keyboard-type         [keyboard-type        (LMGetKbdType)]
                       #:key-translate-options [key-translate-options            #f]
                       #:dead-key-state        [dead-key-state                   #f]  ; no prev state
                       #:keyboard-layout       [layout-in                   'cached]) ; use cached
  (define actual-string-length (box 0))
  (set! key-translate-options
        (or key-translate-options                ; use user settins if provided
            (if dead-key-state                   ; otherwise if user has set dead-key-state,
                0                                ; then take dead-keys into account
                kUCKeyTranslateNoDeadKeysFlag))) ; else ignore dead keys
  (set! dead-key-state (or dead-key-state (box 0)))        ; 0 means no prior key strokes
  (define layout
    (match layout-in
      ; use cached
      ['cached    (cond
                    [cached-keyboard-layout => values]
                    [else   (set! cached-keyboard-layout (get-current-keyboard-layout))
                            cached-keyboard-layout])]
      ; refresh cache
      [#f         (set! cached-keyboard-layout (get-current-keyboard-layout))
                  cached-keyboard-layout]
      ; use provided
      [_          layout-in]))
  
  (UCKeyTranslate layout 
                  virtual-key-code
                  key-action
                  (bitwise-and (>> modifier-key-state 8) #xFF)
                  keyboard-type
                  key-translate-options
                  dead-key-state
                  max-string-length
                  actual-string-length
                  output-chars)
  ; get the number of characters returned, and convert to string
  (define n (max 0 (min max-string-length (unbox actual-string-length))))
  (list->string (for/list ([i (in-range n)]) 
                  (integer->char (ptr-ref output-chars _UniChar i)))))

;;;
;;; RACKET KEY-CODE TO VIRTUAL KEYCODE
;;;

(require mred/private/wx/cocoa/keycode)

(define racket-to-osx-key-code-ht (make-hash))
(for ([c (in-range 256)])
  (define r (map-key-code c))
  (hash-set! racket-to-osx-key-code-ht r c))
;(define (racket-to-osx-key-code k)
;  (define c (hash-ref racket-to-osx-key-code-ht k #f))
  



(module+ test (require rackunit)
  ; These tests are for an US keyboard  
  ; no modifiers
  (check-equal? (key-translate kVK_ANSI_A)      "a")
  (check-equal? (key-translate kVK_ANSI_Period) ".")
  ; shift
  (check-equal? (key-translate kVK_ANSI_A      #:modifier-key-state modifier-shift-key) "A")
  (check-equal? (key-translate kVK_ANSI_Period #:modifier-key-state modifier-shift-key) ">")
  ; option
  (check-equal? (key-translate kVK_ANSI_A      #:modifier-key-state modifier-option-key) "å"))

(module+ danish-test (require rackunit)
  ; These tests are for a DK keyboard  
  ;   Use international settings to add a danish keyboard as an extra text input source.
  ;   Make sure you enable the easy access to switching (flags in the menu bar)
  
  ; no modifiers
  (check-equal? (key-translate kVK_ANSI_A)         "a")
  (check-equal? (key-translate kVK_ANSI_Period)    ".")
  (check-equal? (key-translate kVK_ANSI_Semicolon) "æ")
  ; shift
  (check-equal? (key-translate kVK_ANSI_A         #:modifier-key-state modifier-shift-key) "A")
  (check-equal? (key-translate kVK_ANSI_Semicolon #:modifier-key-state modifier-shift-key) "Æ"))

(module+ chinese-pinyin-test (require rackunit)
  ; These tests are for writing Chinese with the pinyin input method
  ;   Use international settings to add a danish keyboard as an extra text input source.
  ;   Make sure you enable the easy access to switching (flags in the menu bar)
  
  ; no modifiers
  (check-equal? (key-translate kVK_ANSI_A)         "锕")
  (check-equal? (key-translate kVK_ANSI_Period)    ".")
  (check-equal? (key-translate kVK_ANSI_Semicolon) "æ")
  ; shift
  (check-equal? (key-translate kVK_ANSI_A         #:modifier-key-state modifier-shift-key) "A")
  (check-equal? (key-translate kVK_ANSI_Semicolon #:modifier-key-state modifier-shift-key) "Æ"))

(define (make-initial-dead-key-state)
  (box 0))

; Ignore dead keys if dead-key-state isn't provided as optional argument:
(key-translate kVK_ANSI_E #:modifier-key-state modifier-option-key) ; #\́

; A dead key should return an empty string:
(let ([dks (make-initial-dead-key-state)])
  (key-translate kVK_ANSI_E #:dead-key-state dks #:modifier-key-state modifier-option-key)) ; ""

; A dead key ´ followed by e should produce é.
(let ([dks (make-initial-dead-key-state)])
  (key-translate kVK_ANSI_E #:dead-key-state dks #:modifier-key-state modifier-option-key)
  (key-translate kVK_ANSI_E #:dead-key-state dks)) ; "é"

(let ([dks (make-initial-dead-key-state)])
  (key-translate kVK_ANSI_P #:dead-key-state dks)
  (key-translate kVK_ANSI_V #:dead-key-state dks)
  (key-translate kVK_Space  #:dead-key-state dks))

(let ([dks (make-initial-dead-key-state)])
  (list (key-translate kVK_ANSI_A #:dead-key-state dks)
        (key-translate kVK_ANSI_B #:dead-key-state dks)
        (key-translate kVK_Space  #:dead-key-state dks)))

; A dead key ˆ followed by u should produce û.
(let ([dks (make-initial-dead-key-state)])
  (key-translate kVK_ANSI_I #:dead-key-state dks #:modifier-key-state modifier-option-key)
  (key-translate kVK_ANSI_U #:dead-key-state dks)) ; "û"


