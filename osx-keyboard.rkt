#lang racket
(require ffi/unsafe/objc)
(require ffi/unsafe
         ffi/unsafe/objc
         ffi/unsafe/define
         mred/private/wx/cocoa/image
         mred/private/wx/cocoa/types)

;;; One system library is needed:
(define quartz-lib 
  (ffi-lib "/System/Library/Frameworks/Quartz.framework/Versions/Current/Quartz"))
(define-ffi-definer define-quartz quartz-lib)

(import-class NSString)

(tell (tell NSString alloc)
      initWithUTF8String: #:type _string "Hello")

;func UCKeyTranslate(_ keyLayoutPtr: UnsafePointer<UCKeyboardLayout>,
;                  _ virtualKeyCode: UInt16,
;                  _ keyAction: UInt16,
;                  _ modifierKeyState: UInt32,
;                  _ keyboardType: UInt32,
;                  _ keyTranslateOptions: OptionBits,
;                  _ deadKeyState: UnsafeMutablePointer<UInt32>,
;                  _ maxStringLength: UniCharCount,
;                  _ actualStringLength: UnsafeMutablePointer<UniCharCount>,
;                  _ unicodeString: UnsafeMutablePointer<UniChar>) -> OSStatus

(define carbon-core-lib 
  (ffi-lib (string-append "/System/Library/Frameworks/CoreServices.framework/Frameworks/"
                          "CarbonCore.framework/Versions/Current/CarbonCore")))

(define-ffi-definer define-carbon-core carbon-core-lib)

(define carbon-lib (ffi-lib "/System/Library/Frameworks/Carbon.framework/Versions/Current/Carbon"))
(define-ffi-definer define-carbon carbon-lib)

;;; Types from MacTypes.h
(define _UniChar _uint16)
(define _UniCharPointer (_ptr io _UniChar))
(define _UniCharCount _ulong)
(define _UniCharCountPointer (_ptr io _UniCharCount))

(define-carbon UCKeyTranslate
  (_fun (_keyboardLayoutPtr   : (_cpointer 'UCKeyboardLayout))
        (_virtualKeyCode      : _uint16)
        (_keyAction           : _uint16)
        (_modifierKeyState    : _uint32)
        (_keyboardType        : _uint32)
        (_keyTranslateOptions : _uint32)
        (_deadKeyState        : (_ptr io _uint32))
        (_maxStringLength     : _UniCharCount)
        (_actualStringLength  : _UniCharCountPointer)
        (_unicodeString       : _UniCharPointer)
        -> _OSStatus))

(define-carbon TISCopyCurrentKeyboardLayoutInputSource
  (_fun -> (_cpointer 'TISInputSourceRef)))

        

