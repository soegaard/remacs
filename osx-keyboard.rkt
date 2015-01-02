#lang racket
(require ffi/unsafe/objc)
(require ffi/unsafe
         ffi/unsafe/objc
         ffi/unsafe/define
         mred/private/wx/cocoa/image
         mred/private/wx/cocoa/types)

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


(import-class NSString)

;;; API CONSTANTS
(define-carbon kTISPropertyUnicodeKeyLayoutData _NSString)

;;; CORE FOUNDATION

(define _fourchar _uint32)
; (define _OSStatus _sint32) ; already imported
(define _CFIndex _slong)

(define-cpointer-type _CFDataRef)

(define-cf CFRelease (_fun _pointer -> _void))

(define-cf CFDataGetLength
  (_fun _CFDataRef -> _CFIndex))

(define-cf CFDataGetBytePtr
  (_fun _CFDataRef -> _pointer))


(define (CFData->bytes data)
  (let* ([len (CFDataGetLength data)]
         [buf (make-bytes len)]
         [data-ptr (CFDataGetBytePtr data)])
    (memcpy buf data-ptr len)
    buf))

;;; Types from MacTypes.h
(define _UniChar              _uint16)
(define _UniCharCount         _ulong)
(define _UniCharPointer      (_ptr io _UniChar))
(define _UniCharCountPointer (_ptr io _UniCharCount))
(define _OptionBits           _uint32)


;;;
(define _CFStringRef _NSString)

;;; Get the current text input source (read: the keyboard)
(define _TISInputSourceRef (_cpointer 'TISInputSourceRef))
(define-carbon TISCopyCurrentKeyboardLayoutInputSource
  (_fun -> _TISInputSourceRef))

(define the-keyboard (TISCopyCurrentKeyboardLayoutInputSource))
(displayln (list 'keyboard the-keyboard))

;;; Get the layout of the keyboard
(define-carbon TISGetInputSourceProperty
  (_fun (_inputSource : _TISInputSourceRef)
        (_propertyKey : _CFStringRef)           
        -> (_or-null _CFDataRef)))

(define the-keyboard-layout-data
  (TISGetInputSourceProperty the-keyboard kTISPropertyUnicodeKeyLayoutData))
(displayln (list 'layoutData the-keyboard-layout-data))

(define _UCKeyboardLayout (_cpointer 'UCKeyboardLayout))
(define the-keyboard-layout (CFDataGetBytePtr the-keyboard-layout-data))
(cpointer-push-tag! the-keyboard-layout 'UCKeyboardLayout)
(displayln the-keyboard-layout)

(define-carbon LMGetKbdType
  (_fun -> _uint8))


;func UCKeyTranslate(_ keyLayoutPtr:        UnsafePointer<UCKeyboardLayout>,
;                    _ virtualKeyCode:      UInt16,
;                    _ keyAction:           UInt16,
;                    _ modifierKeyState:    UInt32,
;                    _ keyboardType:        UInt32,
;                    _ keyTranslateOptions: OptionBits,
;                    _ deadKeyState:        UnsafeMutablePointer<UInt32>,
;                    _ maxStringLength:     UniCharCount,
;                    _ actualStringLength:  UnsafeMutablePointer<UniCharCount>,
;                    _ unicodeString:       UnsafeMutablePointer<UniChar>) -> OSStatus


(define-carbon UCKeyTranslate
  (_fun (keyboardLayoutPtr   : _UCKeyboardLayout)
        (virtualKeyCode      : _uint16)
        (keyAction           : _uint16)
        (modifierKeyState    : _uint32)
        (keyboardType        : _uint32)
        (keyTranslateOptions : _OptionBits)            ; uint32
        (deadKeyState        : (_ptr i _uint32))
        (maxStringLength     : _UniCharCount)
        (actualStringLength  : _UniCharCountPointer)
        (unicodeString       : _pointer)
        -> _OSStatus))



(define keycode 47)                ; var keycode            = UInt16(kVK_ANSI_Period)
(define keyaction 3)               ; var keyaction          = UInt16(kUCKeyActionDisplay)
(define modifier-key-state 2)      ; var modifierKeyState   = UInt32(1 << 1)             // Shift
(define keyboard-type (LMGetKbdType)) ; var keyboardType    = UInt32(LMGetKbdType())
(define key-translate-options 0)   ;var keyTranslateOptions = OptionBits(kUCKeyTranslateNoDeadKeysBit)
(define dead-key-state 0)          ;var deadKeyState        = UInt32(0)
(define max-string-length 4)       ;var maxStringLength     = UniCharCount(4) // uint32
(define chars (malloc _UniChar 4)) ;var chars: [UniChar]    = [0,0,0,0]
(define actual-string-length 0)    ;var actualStringLength  = UniCharCount(1)

(UCKeyTranslate the-keyboard-layout 
                keycode
                keyaction
                modifier-key-state
                keyboard-type
                key-translate-options
                dead-key-state
                max-string-length
                actual-string-length
                chars)

(displayln (integer->char (ptr-ref chars _UniChar 0)))

