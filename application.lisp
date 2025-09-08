(in-package :cocoagain)

(defconstant +NSActivityIdleDisplaySleepDisabled+ (ash 1 40))
(defconstant +NSActivityIdleSystemSleepDisabled+ (ash 1 20)) 
(defconstant +NSActivitySuddenTerminationDisabled+ (ash 1 14)) 
(defconstant +NSActivityAutomaticTerminationDisabled+ (ash 1 15)) 
(defconstant +NSActivityUserInitiated+ (logior #x00FFFFFF +NSActivityIdleSystemSleepDisabled+)) 
(defconstant +NSActivityUserInitiatedAllowingIdleSystemSleep+ (logand 
							       +NSActivityUserInitiated+
							       (lognot +NSActivityIdleSystemSleepDisabled+))) 
(defconstant +NSActivityBackground+ #x000000FF) 
(defconstant +NSActivityLatencyCritical+ #xFF00000000)

(defconstant +NSApplicationActivationPolicyRegular+ 0)
(defconstant +NSApplicationActivationPolicyAccessory+ 1)
(defconstant +NSApplicationActivationPolicyProhibited+ 2)

(defvar *dispatch-id-map* (make-id-map))
(defvar *widget-id-map* (make-hash-table))
(defvar *startup-hooks* nil)

(cffi:defcallback app-delegate-callback :void ((action :int))
  (case action
    (0 (dolist (hook *startup-hooks*) (funcall hook)))
    (1 ; NB 2025-08-17 10:03:15 is this ever called?
     (let* ((windows (objc
                        (objc "App" "sharedApplication" :pointer)
                        "windows" :pointer)))
         (dotimes (i (objc windows "count" :int))
           (objc (objc windows "objectAtIndex:" :int i :pointer)
                 "performClose:" :pointer (cffi:null-pointer)))))
    (2 (dolist (hook #+sbcl sb-ext:*exit-hooks*) (funcall hook)))))

(cffi:defcallback app-widget-callback :void ((id :pointer))
  (let* ((task (gethash (cffi:pointer-address id) *widget-id-map*))) 
    (when task (handler-case (funcall task id)
                 (error (c)
                   (break (format nil "Caught signal while handling widget callback: \"~a\"" c)))))))

(cffi:defcallback app-dispatch-callback :void ((id :pointer))
  (let* ((id (cffi:pointer-address id))
         (task (id-map-free-object *dispatch-id-map* id)))
    (when task
      (handler-case (funcall task)
        (error (c)
          ;; NB 2025-08-30 06:21:05 I think id is passed as a
          ;; "pointer" but is really an index into *dispatch-id-map*'s
          ;; vector, so value 0 may be legitimate.

          ;; If the thunk has a programming error, it only seems to be
          ;; identified at funcall, not eval. Should be able to [CONTINUE],
          ;; because there's actually no scary null pointer.
          (break (format nil "Caught signal while dispatching event: \"~a\"~%for ~a using ~a.~%Try continuing." c id task)))))))

(defun queue-for-event-loop (thunk)
  (let* ((id (assign-id-map-id *dispatch-id-map* thunk)))
    (cffi:foreign-funcall "dispatch_async_f"
                          :pointer (cffi:foreign-symbol-pointer "_dispatch_main_q") ; queue
                          :pointer (cffi:make-pointer id) ; context
                          :pointer (cffi:callback app-dispatch-callback) ; work
                          )))

(defmacro with-event-loop ((&key (waitp nil)) &body body)
  (alexandria:with-gensyms (result semaphore id) ; semaphore unused
    `(cond ((eql (trivial-main-thread:main-thread) (bt:current-thread))
            (progn ,@body))
           (,waitp (let* ((,result nil)
                          (,id (assign-id-map-id
                                *dispatch-id-map*
                                (lambda () (setf ,result (progn ,@body))))))
                     (cffi:foreign-funcall
                      "dispatch_sync_f"
                      :pointer (cffi:foreign-symbol-pointer "_dispatch_main_q") ; queue
                      :pointer (cffi:make-pointer ,id) ; context
                      :pointer (cffi:callback app-dispatch-callback) ; work
                      )
                     ,result))
           (t (queue-for-event-loop (lambda () ,@body))))))

(let* ((running-p nil)) ; ───────────────────────────────────────────────── Run!
  (defun start-event-loop ()
    (unless running-p
      (defun trivial-main-thread:call-in-main-thread
          ;; double colon for unexported symbol
          (function &key blocking (runner trivial-main-thread::*runner*))
        (declare (ignore runner))
        (with-event-loop (:waitp blocking)
          (funcall function)))
      (trivial-main-thread:swap-main-thread
       (lambda ()
         (setf running-p t)
         (float-features:with-float-traps-masked (:invalid :overflow :divide-by-zero)
           (let* ((pool (new "NSAutoreleasePool"))
                  (ns-app (objc "App" "sharedApplication" :pointer)))
             (enable-foreground)
             (objc ns-app "setDelegateCallback:"
                   :pointer (cffi:callback app-delegate-callback))
             (objc ns-app "setWidgetCallback:"
                   :pointer (cffi:callback app-widget-callback))
             (objc ns-app "setDelegate:"
                   :pointer ns-app)
             (make-default-menubar ns-app)
             (objc ns-app "run")
             (release pool)))))
      :start-event-loop)))

(defun quit ()
  (with-event-loop nil
    (objc (objc "App" "sharedApplication" :pointer)
          "terminate:" :pointer (cffi:null-pointer))))

(defun enable-foreground ()
  (with-event-loop nil
    (objc (objc "App" "sharedApplication" :pointer)
          "activateIgnoringOtherApps:" :bool t)))

(defun set-process-activity (options reason)
  (retain
   (objc (objc "NSProcessInfo" "processInfo" :pointer)
         "beginActivityWithOptions:reason:"
         :unsigned-long-long options
         :pointer (autorelease (make-ns-string reason))
         :pointer)))

(defun prevent-appnap () ; TODO  2025-08-18 21:41:56 not called; check sleep recovery too
  (set-process-activity
   (logior +NSActivityUserInitiated+ +NSActivityLatencyCritical+)
   "Live feels"))

(defun make-menu-item (name &key action key)
  (objc (alloc "NSMenuItem")
        "initWithTitle:action:keyEquivalent:"
        :pointer (autorelease (make-ns-string name))
        :pointer (sel action)
        :pointer (autorelease (make-ns-string key))
        :pointer))

(defun make-default-menubar (ns-app)
  (let* ((menubar (autorelease (new "NSMenu")))
         (app-menu-item (autorelease (new "NSMenuItem")))
         (edit-menu-item (autorelease (new "NSMenuItem"))))
    (objc ns-app "setMainMenu:" :pointer menubar)
    (objc menubar "addItem:" :pointer app-menu-item)
    (objc menubar "addItem:" :pointer edit-menu-item)
    (let* ((app-menu (autorelease (new "NSMenu")))
           (quit-menu-item
             (autorelease
              (make-menu-item
               (ns-string-to-lisp
                (objc (objc "NSProcessInfo" "processInfo" :pointer)
                      "processName" :pointer))
               :action "terminate:" :key "q"))))
      (objc app-menu "addItem:" :pointer quit-menu-item)
      (objc app-menu-item "setSubmenu:" :pointer app-menu))
    (let* ((edit-menu (autorelease
                       (objc
                        (alloc "NSMenu")
                        "initWithTitle:"
                        :pointer (autorelease (make-ns-string "Edit"))
                        :pointer)))
           (close-menu-item (autorelease
                             (make-menu-item
                              "Close"
                              :action "performClose:" :key "w")))
           (fullscreen-menu-item (autorelease
                             (make-menu-item
                              "Toggle Fullscreen"
                              :action "toggleFullscreen" :key "f")))
           (hide-menu-item (autorelease
                             (make-menu-item
                              "Hide"
                              :action "hide:" :key "h"))))
      (objc edit-menu "addItem:" :pointer hide-menu-item)
      (objc edit-menu "addItem:" :pointer close-menu-item)
      (objc edit-menu "addItem:" :pointer fullscreen-menu-item)
      (objc edit-menu-item "setSubmenu:" :pointer edit-menu))))
