(ql:quickload '(:telnetlib
                :cl-ppcre
                :bordeaux-threads
                :inferior-shell))

;; doll.lisp for lambdamoo                                                                                                                                                                                                                                                                                                                                                                
;; dimension on-off lisp library? done over lisp lambdamoo? idk yet.                                                                                                                                                                                                                                                                                                                      
;; written by elsa(#125418)@lambdamoo from 2024.02.04 [ marsipan@de1.hashbang.sh ]                                                                                                                                                                                                                                                                                                        

    #| need a package definition and proclaimataions |#


(in-package :cl-user)


(use-package :telnetlib)
(use-package :sb-thread)
(use-package :cl-ppcre)

    #| the parameters can go in a config file |#

(defparameter *host* "lambda.moo.mud.org")
(defparameter *port* 8888)

(defparameter *telnet* nil)
(defparameter *thread* nil)
(defparameter *listener-thread* nil)
(defparameter *repl-thread* nil)

(defparameter *login* "elsa")
(defparameter *pass* "")

(defparameter *heartbeat-sleep* (* 60 60.0))
(defparameter *doll-local-message-prefix* " (:doll) [local] %  ")

(defparameter *afk* nil)
(defparameter *ooc* nil)

(defparameter *dolldb* '((1 . 125418)))

    #| doll needs to be simple and expansive                                                                                                                                                                                                                                                                                                                                              
       expansions can happen outside the main file |#

(defun connect-to-host ()
  (open-telnet-session *host* *port*))

(defun format-something-to-lambdamoo (arg)
  (format-tn *telnet*  "~a~%" arg))

(defun lm (arg)
  (format-something-to-lambdamoo arg))
(defparameter format-lm #'format-something-to-lambdamoo)
(setf (symbol-function 'format-lm) #'format-something-to-lambdamoo)

;; aliasing, kinda cleaning up for the package. now we need a real hacky form.                                                                                                                                                                                                                                                                                                            

(defun format-lm* (&key arg verb iobj prep dobj (space #\Space) (repeat 0) (suspend 0))
  (let ((args
          (if arg arg
              (format nil "~a~C~a~C~a~C~a~%"
                      (if verb verb "")
                      space
                      (if dobj dobj "")
                      space
                      (if prep prep "")
                      space
                      (if iobj iobj "")))))
    (format-lm args)
    (sleep suspend))
  (unless (equal repeat 0)
    (format-lm* :arg arg :verb verb :iobj iobj :prep prep :dobj dobj :repeat (- repeat 1) :suspend suspend)))

;; what the fuck are you doing zoe[umlaut] ;; ooo write a macrochar for uml -- whhy double comment                                                                                                                                                                                                                                                                                        
#|                                                                                                                                                                                                                                                                                                                                                                                        
(format-lm* :arg "say imogenedoll - i'm already looping this so let's hope i'm not triggering a recursive thing i didn't detect during my just-now readthrough of my now-not-very-hacky code" :repeat (+ (random 5) 5) :suspend (+ 2 (random 2)))                                                                                                                                         
                                                                                                                                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                                                                                                                                          
(format-lm* :verb "bet" :dobj "1000" :prep "on" :iobj (string (random 35))                                                                                                                                                                                                                                                                                                                
            :repeat 10 :suspend (+ 30 (random 5))))                                                                                                                                                                                                                                                                                                                                       
                                                                                                                                                                                                                                                                                                                                                                                          
(format-lm* :verb "bet" :dobj "100" :prep "on" :iobj "war"                                                                                                                                                                                                                                                                                                                                
            :repeat 500 :suspend (+ 1 (random 5))))                                                                                                                                                                                                                                                                                                                                       
                                                                                                                                                                                                                                                                                                                                                                                          
(progn                                                                                                                                                                                                                                                                                                                                                                                    
(format-lm "@go #5198")                                                                                                                                                                                                                                                                                                                                                                   
(sleep 1)                                                                                                                                                                                                                                                                                                                                                                                 
(format-lm "punch clock")                                                                                                                                                                                                                                                                                                                                                                 
(sleep 1)                                                                                                                                                                                                                                                                                                                                                                                 
(format-lm "home"))                                                                                                                                                                                                                                                                                                                                                                       
|#
(defun parse-a-line-for-doll ()
  #| write a doll verb on yourself at lambdamoo                                                                                                                                                                                                                                                                                                                                           
     so `doll elsa' will notify elsa data                                                                                                                                                                                                                                                                                                                                                 
     peek at available telnet data                                                                                                                                                                                                                                                                                                                                                        
     collect ones that look like                                                                                                                                                                                                                                                                                                                                                          
    `doll % elsa . (#125418) % imogene . (#125450) % 1707213632                                                                                                                                                                                                                                                                                                                           
  |#)

(defun local-doll-line (argstr)
  (format t "~%~a~a~%" *doll-local-message-prefix* argstr))
(defun toggle-afk ()
  (setf *afk* (if *afk* nil t))
  (format t "~% afk toggled to: ~a~%" *afk*))

(defun toggle-ooc ()
  (setf *ooc* (if *ooc* nil t))
  (format t "~% ooc toggled to: ~a~%" *ooc*))

(defun eval-to-lambdamoo (arg)
  (format-something-to-lambdamoo (eval arg)))

#| there is something wrong with the toggles but i think i fixed it |#

(defun local-heartbeat ()
  (format t "~%  doll  >> ~d~%" (sb-posix:time)))

(defun actual-local-heartbeat ()
  (if (equal *heartbeat-sleep* 0.0)
      (format t "~% turned off heartbeat ~%")
      (progn
        (local-heartbeat)
        (sleep *heartbeat-sleep*)
        (actual-local-heartbeat))))

;(actual-local-heartbeat)                                                                                                                                                                                                                                                                                                                                                                 



(defun chew ()
  (lm ": ----------------------------------------------------------------")
  (lm ": ---- doll ---- chewing imogene for your enjoyment --------------")
  (lm ": ----------------------------------------------------------------")
  (format-something-to-lambdamoo
   (format nil ": ---- [OOC/IC] ---- imogene is currently ~ain character."
           (if *ooc* "not " "")))
  (format-something-to-lambdamoo
   (format nil ": ---- [AFK/AK] ---- imogene is currently ~a."
           (if *afk* "away from the keyboard" "hacking at terminal")))
  (lm ": ----------------------------------------------------------------"))

#| fix the above. it can't handle the body list for the format form. |#

(defun setup-for-eval (arg)
  (format t "~%~%DOLL~%~%~a~%~%" arg))

(defun reverse-string (argstr)
  (reverse argstr))

(defun reverse-say (argstr)
  (format-tn *telnet* "say ~a~%" (reverse argstr)))

(defun kanda-response (argstr)
  (let ((response (inferior-shell:run/ss (format nil "anneliza-1086-b ~C (the following is sent from lambdamoo via zoe/librecell doll client ... your reply will be printed as 'kanda replies: (reply)' so please make sure not to add a redundant 'kanda' or 'reply' etc.): ~a ~C" #\" argstr #\") :on-error nil))) (format-tn *telnet* "com #[REMOVED BY ZOE FOR SECURITY REASONS] to emote replies: ~a~%" (string-downcase respons\
e))))

#|(defun format-to-players-in-room (string)
  (let ((contents (progn (format-lm "#here"))                                                                                                                                                                                                                                                                                                                                             
                  (peek-available-data *telnet*)))                                                                                                                                                                                                                                                                                                                                        
    (values contents string)))|#


(defparameter *current-storage* nil)

#|                                                                                                                                                                                                                                                                                                                                                                                        
(progn                                                                                                                                                                                                                                                                                                                                                                                    
  (format-tn *telnet* "~%~C(here).contents~%" #\;)                                                                                                                                                                                                                                                                                                                                        
0  (sleep 0.05)                                                                                                                                                                                                                                                                                                                                                                           
  (setf *current-storage*                                                                                                                                                                                                                                                                                                                                                                 
        (string (telnetlib::peek-available-data *telnet*))))                                                                                                                                                                                                                                                                                                                              
;  (loop for i in *current-storage* collect                                                                                                                                                                                                                                                                                                                                               
;       (format-tn *telnet* "~%~C~d:tell('this is a string')~%" #\; i)))                                                                                                                                                                                                                                                                                                                  
|#


;(progn                                                                                                                                                                                                                                                                                                                                                                                   
;  (fmakunbound #'parse-for-reverse)                                                                                                                                                                                                                                                                                                                                                      
;  (fmakunbound #'kanda-response))                                                                                                                                                                                                                                                                                                                                                        


(defun parse-for-reverse ()
  (let ((data (peek-available-data *telnet*)))
    (when (cl-ppcre:scan "eval\\s+([\\w|\\s]*)" data)
      (setup-for-eval "\\1"))
    (when (cl-ppcre:scan "ZOE REMOVED THIS PART OF THIS FORM FOR SECURITY REASONS" data)
      (local-heartbeat)
      (kanda-response data))
    (when (cl-ppcre:scan "doll.*>>.*chew.*.*imogene" data)
      (chew))
    (when (cl-ppcre:scan "doll.*toggle.*afk" data)
      (toggle-afk))
    (when (cl-ppcre:scan "ays, \"elsa!\"" data)
      (progn
        (format-tn *telnet* ";#125418:tell(\"[~d] doll needs you!\")~%" (get-universal-time))
        (format-tn *telnet* "~%say [~d] doll paged elsa on her mobile device!~%" (get-universal-time))))
    (when (cl-ppcre:scan "imo.*afkp" data)
      (format-tn *telnet* "~%say my afk is set to: ~a~%" *afk*))
    (when (cl-ppcre:scan "doll.*toggle.*ooc" data)
      (toggle-ooc))1
    (when (cl-ppcre:scan "imogenedoll" data)
      (local-heartbeat)
      (lm "say doll caught a name drop"))
    (when (cl-ppcre:scan "doll: reverse" data)
      (local-heartbeat)
      (reverse-say (remove #\Newline (subseq data (+ (position #\: data) 10)  (- (length data) 2)))))
    (when (cl-ppcre:scan "doll: what is the lisp timestamp?" data)
      (format-something-to-lambdamoo (format nil "say ~d" (get-universal-time))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                                                                                                                                                                                                                                                                                          

  (let ((contents (progn (format-lm "#here"))                                                                                                                                                                                                                                                                                                                                             
                  (peek-available-data *telnet*)))                                                                                                                                                                                                                                                                                                                                        
    (values contents string)))|#


(defparameter *current-storage* nil)

#|                                                                                                                                                                                                                                                                                                                                                                                        
(progn                                                                                                                                                                                                                                                                                                                                                                                    
  (format-tn *telnet* "~%~C(here).contents~%" #\;)                                                                                                                                                                                                                                                                                                                                        
0  (sleep 0.05)                                                                                                                                                                                                                                                                                                                                                                           
  (setf *current-storage*                                                                                                                                                                                                                                                                                                                                                                 
        (string (telnetlib::peek-available-data *telnet*))))                                                                                                                                                                                                                                                                                                                              
;  (loop for i in *current-storage* collect                                                                                                                                                                                                                                                                                                                                               
;       (format-tn *telnet* "~%~C~d:tell('this is a string')~%" #\; i)))                                                                                                                                                                                                                                                                                                                  
|#


;(progn                                                                                                                                                                                                                                                                                                                                                                                   
;  (fmakunbound #'parse-for-reverse)                                                                                                                                                                                                                                                                                                                                                      
;  (fmakunbound #'kanda-response))                                                                                                                                                                                                                                                                                                                                                        


(defun parse-for-reverse ()
  (let ((data (peek-available-data *telnet*)))
    (when (cl-ppcre:scan "eval\\s+([\\w|\\s]*)" data)
      (setup-for-eval "\\1"))
    (when (cl-ppcre:scan "kanda35" data)
      (local-heartbeat)
      (kanda-response data))
    (when (cl-ppcre:scan "doll.*>>.*chew.*.*imogene" data)
      (chew))
    (when (cl-ppcre:scan "doll.*toggle.*afk" data)
      (toggle-afk))
    (when (cl-ppcre:scan "ays, \"elsa!\"" data)
      (progn
        (format-tn *telnet* ";#125418:tell(\"[~d] doll needs you!\")~%" (get-universal-time))
        (format-tn *telnet* "~%say [~d] doll paged elsa on her mobile device!~%" (get-universal-time))))
    (when (cl-ppcre:scan "imo.*afkp" data)
      (format-tn *telnet* "~%say my afk is set to: ~a~%" *afk*))
    (when (cl-ppcre:scan "doll.*toggle.*ooc" data)
      (toggle-ooc))1
    (when (cl-ppcre:scan "imogenedoll" data)
      (local-heartbeat)
      (lm "say doll caught a name drop"))
    (when (cl-ppcre:scan "doll: reverse" data)
      (local-heartbeat)
      (reverse-say (remove #\Newline (subseq data (+ (position #\: data) 10)  (- (length data) 2)))))
    (when (cl-ppcre:scan "doll: what is the lisp timestamp?" data)
      (format-something-to-lambdamoo (format nil "say ~d" (get-universal-time))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                                                                                                                                                                                                                                                                                          

#| registration with credentials |#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                                                                                                                                                                                                                                                                                          


(defun listener-loop ()
  (parse-for-reverse)
  (read-available-data *telnet*)
  (sleep 1) ;;worst line in the program. fix this elly.                                                                                                                                                                                                                                                                                                                                   
  (listener-loop))

(defun listener-thread ()
  (setf *listener-thread*
        (make-thread #'listener-loop :name "doll listener thread")))

(defparameter *doll-commands* nil)

#|(defun note-to-self (&optional name)                                                                                                                                                                                                                                                                                                                                                    
  (declare (ignore name))                                                                                                                                                                                                                                                                                                                                                                 
  (let ((name (when name name)                                                                                                                                                                                                                                                                                                                                                            
              (get-univiersal-timestamp)))))|#

(defun process-as-doll-local-command (thingie)
  (let ((new-thingie (subseq thingie 1)))
    (when (equal (subseq new-thingie 0 4) "rsay")
      (format-lm (concatenate 'string "say " (reverse (subseq new-thingie 5)))))
    (when (equal (subseq new-thingie 0 4) "loop")
      (let ((form (progn (princ "iterate what? ")
                         (read-line)))
            (ions (progn (princ "how many? ")
                         (read)))
            (wait (progn (princ "seconds between? ")
                         (read))))
        (sb-thread:make-thread (lambda () (format-lm* :arg form :repeat ions :suspend wait)))))
    (when (equal (subseq new-thingie 0 4) "eval")
      ())
    (when (and (> (length new-thingie) 5) (equal (subseq new-thingie 0 6) "random"))
      (progn
        (let* ((ceiling (progn (princ "random number ceiling? ")
                               (read)))
               (number (random ceiling)))
          (format t "~d" number))
        nil))
    (format t "~a~%"new-thingie)))

(defun handle-local (thingie)
  (process-as-doll-local-command thingie))

(defun repl-loop ()
  (let ((next (read-line)))
    (when (string-equal next "/quit")
      nil)
    (if (and next (equal (position #\/ next) 0))
        (handle-local next)
        (format-something-to-lambdamoo next)))
    (repl-loop))


  #| need to be able to jump in and out of the repl loop. |#
(defun repl-thread ()
  (setf *repl-thread*
        (make-thread #'repl-loop :name "doll read/format loop")))

(defun start-doll ()
  (setf *telnet* (connect-to-host))
  (listener-thread)
  (repl-loop))



#|                                                                                                                                                                                                                                                                                                                                                                                        
(defun begin-doll ()                                                                                                                                                                                                                                                                                                                                                                      
  (setf *telnet* (connect-to-host))                                                                                                                                                                                                                                                                                                                                                       
  (read-available-data *telnet*)                                                                                                                                                                                                                                                                                                                                                          
  (format-tn *telnet* "help")                                                                                                                                                                                                                                                                                                                                                             
  (read-available-data *telnet*)                                                                                                                                                                                                                                                                                                                                                          
  (close-telnet-session *telnet*))                                                                                                                                                                                                                                                                                                                                                        
                                                                                                                                                                                                                                                                                                                                                                                          
(defun hackyshit ()                                                                                                                                                                                                                                                                                                                                                                       
  (read-available-data tem)                                                                                                                                                                                                                                                                                                                                                               
  (sleep 1)                                                                                                                                                                                                                                                                                                                                                                               
  (hackyshit))                                                                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                                                                          
(defun hacky-doll-thread ()                                                                                                                                                                                                                                                                                                                                                               
  (setf *thread*                                                                                                                                                                                                                                                                                                                                                                          
        (make-thread #'hackyshit :name "ily")))                                                                                                                                                                                                                                                                                                                                           
                                                                                                                                                                                                                                                                                                                                                                                          
(defun lamloop ()                                                                                                                                                                                                                                                                                                                                                                         
  (let ((next (read-line)))                                                                                                                                                                                                                                                                                                                                                               
    (lam next)                                                                                                                                                                                                                                                                                                                                                                            
    (lamloop)))                                                                                                                                                                                                                                                                                                                                                                           
                                                                                                                                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                                                                                                                                          
ame ga takusan futte iru                                                                                                                                                                                                                                                                                                                                                                  
node soto de odoru hitsuyou ga                                                                                                                                                                                                                                                                                                                                                            
aru to omimasu                                                                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                                                                          
|#







