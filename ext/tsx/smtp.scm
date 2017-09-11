; smtp.scm
; Sample usage of TinyScheme Extensions
; This very simple program sends a message using SMTP to the local machine.

; It uses the following TinyScheme Extension functions:
;     getenv 
;         used to get name of current user, wich is the recipient
;         of the message.
;     make-client-socket
;         used to connect to SMTP port on local machine
;     send
;         used to send commands and email message
;     recv-new-string
;         used to read responses from SMTP server
;     close-socket
;         used to free socket at the end

; check that string ports are available...
(if (not (defined? 'open-output-string))
  (begin
    (display "We need string ports!! Recompile TinyScheme with string ports, if you want to run this sample...")
    (quit)))

; check that extensions are enabled
(if (not (defined? 'load-extension))
    (begin
      (display "TinyScheme has extensions disabled. Enable them!!")
      (newline)
      (quit)))

; load TinyScheme Extensions
(load-extension "tsx-1.0/tsx")

; check that the necessary functions are available (the user
; might have removed some functionality...)
(if (or
      (not (defined? 'getenv))
      (not (defined? 'make-client-socket))
      (not (defined? 'send))
      (not (defined? 'close-socket))
      (not (defined? 'recv-new-string)))
    (begin
      (display "Some necessary functions are not available. Exiting!")
      (newline)
      (quit)))

; get current user name
(define user-name (getenv "USER"))

; if unable to get user name, use "nobody"
(if (not user-name)
  (set! user-name "nobody"))

; create client socket to SMTP port (25)
(define sock (make-client-socket "localhost" 25))
(display "Socket: ") (display sock) (newline)

; if unable to open socket, exit TinyScheme
(if (not sock)
  (begin
    (display "Unable to open socket! Is SMTP enabled on this machine?")
    (quit)))

; define string buffers to send and receive
(define recv-buf '())

; receive SMTP welcome message onto recv-buf var
(set! recv-buf (recv-new-string sock))

(display "Received:") (display recv-buf) (newline)

(define helo "HELO localhost\n")
(display "Sending HELO...") (newline)
(send sock helo)

; receive response from server
(set! recv-buf (recv-new-string sock))
(display "Received:") (display recv-buf) (newline)

(define mailfrom (make-string (+ 20 (string-length user-name))))
(define mailfromport (open-output-string mailfrom))
(display "MAIL FROM: " mailfromport)
(display user-name mailfromport)
(display "\n" mailfromport)
(close-output-port mailfromport)

; send MAIL FROM: command and receive response
(display "Sending MAIL FROM:...") (newline)
(send sock mailfrom)
(set! recv-buf (recv-new-string sock))
(display "Received:") (display recv-buf) (newline)

; send RCPT TO: command and receive response
(display "Sending RCPT TO:...") (newline)
(define rcptto (make-string (+ 20 (string-length user-name))))
(define rcpttoport (open-output-string rcptto))
(display "RCPT TO: " rcpttoport)
(display user-name rcpttoport)
(display "\n" rcpttoport)
(close-output-port rcpttoport)
(send sock rcptto)
(set! recv-buf (recv-new-string sock))
(display "Received:") (display recv-buf) (newline)


; send DATA command
(display "Sending DATA...") (newline)
(define data "DATA\n")
(send sock data)
(set! recv-buf (recv-new-string sock))
(display "Received:") (display recv-buf) (newline)

; send message
(display "Sending message...") (newline)
(define message "Hello!\nThis is a sample message sent from TinyScheme!\n\n.\n")
(send sock message)
(set! recv-buf (recv-new-string sock))
(display "Received:") (display recv-buf) (newline)

; send QUIT command
(display "Sending QUIT command...") (newline)
(define quit "QUIT\n")
(send sock quit)
(set! recv-buf (recv-new-string sock))
(display "Received:") (display recv-buf) (newline)

; close socket
(close-socket sock)

