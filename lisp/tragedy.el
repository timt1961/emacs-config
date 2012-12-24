;;- tragedy.el --- ...that we've lost the text adventures of yesteryear

;; Copyright (C) 1999 John Wiegley.

;; Author: John Wiegley <johnw@oneworld.new-era.com>
;; Created: 20 Jan 1999
;; Version: 1.0
;; Keywords: games

;;; Commentary:
;;
;; This engine is intended to make the creation of text adventures easy
;; and fun.  For everyone who has ever wanted to create their own
;; world...

;;; Model of the Universe:
;;
;; In these tragedies, the world is composed of objects.  Each object
;; lives in a place.  Places are generally associated with one another,
;; allowing for the movement of a object between them.
;;
;; A place needn't be a room: it can be the inside of a pocket -- which
;; means that a place might be located within an object!
;;
;; Every object and place has state (aka attributes).  Every change in
;; state constitutes an event.  Some events are no more complex than the
;; state change itself; while others may result in further changes of
;; state -- possibly those of other objects and places.  This chain
;; reaction continues until no more events occur.  Beware of recursion
;; when programming your events!
;;
;; Events occur due to actions.  Any object may perform an action,
;; whether through human intervention, or an AI routine.  There is also
;; one, non-object actor: Time.  This actor performs actions
;; universally, without requiring an object.
;;
;; The actual "playing" of the game consists of provoking events through
;; actions, and then discovering, via descriptive text, how the world
;; evolves due to those changes in state.
;;
;; Finally, any .TDY file represents the state of a given universe at a
;; particular moment in time, combined with a grammar allowing for the
;; translation of human intent into object-based actions.

;;; Conventions:
;;
;; Some standard notions used by this engine:
;;
;;   You specify for every place which objects it contains.  You *don't*
;;   specify for an object which place it's in.  This is a six-of-one,
;;   half-dozen-of-the-other problem.  I finally decided on the former
;;   because it results in the information being more centralized.
;;
;;   Use 'tdy-connect' to bind two places together if the association is
;;   meant to be two-way.  This saves typing, since you don't have to
;;   specify in both places a reference to other.  For one-way
;;   associations, however, you should still set 't:connections'
;;   normally.
;;
;; There are a few attributes which have special meaning (i.e., the game
;; engine uses their values).  They all begin with "t:" to donate that
;; their specialness.  None of these special attributes are strictly
;; required to be set by the user.  Of those that should or ought to be,
;; they are:
;;
;;   t:defa  "definite article"      (typically "the", or nothing)
;;   t:defi  "indefinite article     ("a", "an", or nothing)
;;   t:sdesc "One-liner description" (eq tdy-verbosity 'verbose)
;;   t:fdesc "Complete description"  (or (eq t:seen nil)
;;                                       (eq tdy-verbosity 'super-verbose))
;;
;; Note that "t:sdesc" and "t:fdesc" may be set to many different
;; values.  Either a string, a function (it will call the function with
;; the current object/place as its argument), a lisp form, a variable...
;; it will evaluate the value accordingly, to determine what the
;; description text should be.  Also, if "t:defi" has no value, then the
;; definite article will always be used.
;;
;; For places only, these two additional properties may be set:
;;
;;   t:contents    ("Titles of objects" ...)
;;   t:connections (("direction" . "Title of place") ...)
;;
;; Note that "t:connections" is usually handled by 'tdy-connect', and 

;;; Internationalization:
;;
;; For the purposes of making this engine useful to non-English speaking
;; players, all strings that might be displayed to the user are
;; contained in global variables which should be redefined in the game
;; text, if that game text is non-English.

(defvar tdy-intro nil
  "Welcome to the game!  This text will be displayed at the very
beginning, if set to anything.")

(defvar tdy-objects '()
  "A list of all the object symbols in the game.")

(defvar tdy-places '()
  "A list of all the place symbols in the game.")

(defvar tdy-connections '()
  "A pending list of connections to be built up.  Used only at
initialization time.")

(defvar tdy-events '()
  "See the documentation for 'tdy-event'.")

(defvar tdy-self nil
  "This is the 'self' object, the main perspective of the human doer.")

(defvar tdy-mode-map nil
  "A mode map for catching the RET key in TDY mode.")

(defvar tdy-verbosity 'verbose
  "What is the verbosity level?  Could be 'verbose, 'super-verbose, or
'brief.")

(defvar tdy-error-flag nil
  "Set to 't' whenever an error is triggered.")

(defvar tdy-prepositions nil
  "A list of all the recognized prepositions.")

(defvar tdy-grammar nil
  "The grammar by which human intentions are converted into actions,
performed by the tdy-self object.")

(defvar tdy-current-place nil
  "Name of the place where tdy-self is located.  This is included in the
modeline.")

(defvar tdy-errors
  '(
    (badcmd   . "Command was incomprehensible.")
    (nosym    . "There is no symbol named \"%s\"!")
    (badmove  . "You can't go that way.")
    (unownobj . "Object \"%s\" is not contained anywhere!")
    ))

(defun tdy-sym (entity &optional create)
  "Return the TDY game symbol which corresponds to TITLE.  If TITLE is
already a symbol (which means that somebody above us already resolved
it), then simply return the symbol."
  (if (symbolp entity)
      entity
    (let ((name entity) obj)
      (while (string-match " " name)
        (setq name (replace-match "-" t t name)))
      (setq name (concat "tv:" name))
      (setq obj (if create
                    (intern name)
                  (intern-soft name)))
      (if (not obj)
          (tdy-err "sym" 'nosym entity))
      obj)))

(defun tdy-match (func item obj)
  "See if ITEM is in OBJ, using FUNC.  OBJ can be either an atom or
list."
  (if (not (listp obj))
      (funcall func obj item)
    (catch 'found
      (mapcar (function
               (lambda (elem)
                 (if (funcall func elem item)
                     (throw 'found t))))
              obj)
      nil)))

;; NYI: add support for groupings, so that, for example, one could check
;; for a specific event against all of certain type of object or place.

(defun tdy-attr (entity attr)
  "Return the value of the ATTR attribute for ENTITY."
  (get (tdy-sym entity) attr))

(defun tdy-title (entity)
  "Return the title of ENTITY."
  (tdy-attr entity 't:title))

(defun tdy-location (entity)
  "Return the place of ENTITY."
  (tdy-attr entity 't:place))

(defun tdy-noun (entity &optional indefinite capitalize)
  "Return the noun form name of ENTITY.  That is, if it has a definite
or indefinite article associated with it, then return the text form of
the entity's name, including the appropriate article."
  (let (art)
    (if (and indefinite
             (setq art (tdy-attr entity 't:defi)))
        t
      (setq art (tdy-attr entity 't:defa)))
    (if art
        (concat (if capitalize (upcase-initials art) art)
                " " (tdy-title entity))
      (tdy-title entity))))

(defun tdy-change (entity attr val)
  "Change the given attribute for the entity to VAL.  This will trigger
any related events.  Do NOT just use 'put', which would be bad bad bad."
  (let* ((this (tdy-sym entity))
         (cur (get this attr))
         (events (cdr (assq this tdy-events))))

    ;; set the attribute on the symbol.  This might have been either a
    ;; place, or an object.
    (put this attr val)

    ;; now scan the events list, to see if anyone has been waiting for
    ;; this to happen.
    (let ((count 0))
      (if events
          (mapcar
           (function
            (lambda (entry)
              (let* ((check (car entry))
                     (lambda-form (eq (car check) 'lambda))
                     (run-event
                      (if lambda-form
                          (funcall check this)
                        (and (tdy-match 'eq attr (car check))
                             (tdy-match 'equal val (eval (cadr check)))
                             (or (not (caddr check))
                                 (tdy-match 'equal cur
                                            (eval (caddr check))))))))
                (if run-event
                    (progn
                      (setq count (1+ count))
                      (if lambda-form
                          (funcall (cadr entry) this)
                        (funcall (cadr entry) this val cur)))))))
           events))
      count)))

(defun tdy-def (title attrs)
  "Define an object/entity which will participate in the adventure."
  (let ((obj (tdy-sym title t)))
    (setplist obj attrs)
    (put obj 't:title title)
    obj))

(defun tdy-object (title attrs)
  "Define a new object.  This is the basic building block!"
  (let ((sym (tdy-def title attrs)))
    (setq tdy-objects
          (cons sym tdy-objects))))

(defun tdy-place (title attrs)
  "Define a new place, in which to place objects."
  (let ((sym (tdy-def title attrs)))
    (put sym 't:locp t)
    (setq tdy-places
          (cons sym tdy-places))))

(defun tdy-connect (joints)
  "Connect together a series of places.  The basic format is:
    (FROM-WHERE HOW-TO-GO-TO TO-WHERE HOW-TO-GET-BACK)
By using connections, it eleminates the redundancy of having to specify
cross-references all over the place."
  (setq tdy-connections
        (nconc tdy-connections joints)))

(defun tdy-event (title watches)
  "Define an event, which is an occurance based on a change of state in
an object.  Each event corresponding to a tdy-change on a certain object
or place.  The format is a list of change descriptions, which assume the
form:

    ((CHANGE FUNCTION) ...)

If CHANGE is true (see below), FUNCTION is called with either one, or
three arguments.  If the CHANGE was a lambda form, then FUNCTION will
simply be called with the object for which the event is being defined.
However, if CHANGE is an attribute-watch, then the first argument will
be the object for which the event is defined, the second will be the
previous value of that attribute, and the third, the current value.

CHANGE is either a lambda form, taking the object considered as its
argument, which will be called to determine if the result is non-nil; or
it is a special form with the following syntax:

    (ATTR TO &option FROM)

ATTR is the unquoted(!) name of the attribute being watched.  TO is the
value or list of possible values which the event is expected to change
to.  FROM is the value or list of possible values which the event is
required to change from, if present.  Both TO and FROM may be functions,
lists, variables, etc.  The forms will simply be eval'd, so remember to
use quoting if necessary."
  (let* ((sym (tdy-sym title))
         (elem (assq sym tdy-events)))
    (if elem
        (setcdr elem (nconc (cdr elem) watches))
      (setq tdy-events
            (cons (cons sym watches)
                  tdy-events)))))

(defun tdy-located (obj loc)
  "Is OBJ located in LOC?"
  (let ((osym (tdy-sym obj))
        (lsym (tdy-sym loc)))
    (eq (get osym 't:place) lsym)))

(defun tdy-contains (loc obj)
  "Does LOC contain OBJ?"
  (let ((osym (tdy-sym obj))
        (lsym (tdy-sym loc)))
    (memq osym (get lsym 't:contents))))

(defun tdy-init ()
  "Establish all containership links between all objects and places.
This function also has the responsibility of turning all textual entity
references into actual symbol references."
  (setq tdy-error-flag nil)

  ;; Make sure that the contents of every place has a t:place attribute
  ;; pointing back to that place.  Also, the contents list is
  ;; transformed into a list of symbols, rather than a list of strings.
  (mapcar
   (function
    (lambda (loc)
      (let (objs)
        (mapcar
         (function
          (lambda (title)
            (let ((obj (tdy-sym title)))
              (put obj 't:place loc)
              (setq objs (cons obj objs)))))
         (get loc 't:contents))
        (put loc 't:contents objs))))
   tdy-places)

  ;; Verify that every object in the game is now located somewhere.
  ;; Since this is only an error check, it could actually be omitted...
  (mapcar
   (function
    (lambda (obj)
      (if (not (get obj 't:place))
          (tdy-err "init" 'unownobj (tdy-title obj)))))
   tdy-objects)

  ;; Bind all the connections together that were declared.
  (mapcar
   (function
    (lambda (joint)
      (let* ((from (car joint))
             (how  (downcase (cadr joint)))
             (to   (caddr joint))
             (back (downcase (cadddr joint)))
             (fsym (tdy-sym from))
             (tsym (tdy-sym to)))
        (put fsym 't:connections
             (cons (cons how tsym) (get fsym 't:connections)))
        (put tsym 't:connections
             (cons (cons back fsym) (get tsym 't:connections))))))
   tdy-connections)

  (setq tdy-connections '())

  ;; Set the self object to its actual symbol.
  (setq tdy-self (tdy-sym tdy-self))

  (not tdy-error-flag))

(defun tdy-quit ()
  "Delete all of the symbols which were created during this run."

  (mapcar (function (lambda (obj)
                      (if (symbolp obj)
                          (unintern obj))))
          tdy-objects)

  (mapcar (function (lambda (loc)
                      (if (symbolp loc)
                          (unintern loc))))
          tdy-places)

  (setq tdy-objects      '())
  (setq tdy-places       '())
  (setq tdy-events       '())
  (setq tdy-intro        nil)
  (setq tdy-self         nil)
  (setq tdy-intro        nil)
  (setq tdy-verbosity    'verbose)
  (setq tdy-prepositions nil)
  (setq tdy-grammar      nil))

(defun tdy-msg (&rest args)
  "Print out a message to the tragic display."

  ;; Jump to the end of the buffer
  (goto-char (point-max))

  ;; Move to the beginning of the line
  (let ((beg (point)))
    (beginning-of-line)

    ;; make sure the message starts on a new line
    (if (< (point) beg)
        (progn
          (goto-char beg)
          (insert "\n")))
    (setq beg (point))

    ;; output the arguments followed by a new line
    (mapcar (function (lambda (arg)
                        (insert arg)))
            args)
    (insert "\n")

    ;; go back and fill the paragraph
    (fill-individual-paragraphs beg (point))))

(defun tdy-err (where code &rest args)
  "Display an error message to the user."
  (setq tdy-error-flag t)
  (let (msg
        (err
         (if (not (stringp code))
             (assq code tdy-errors)
           nil)))
    (if (not err)
        (if (stringp code)
            (setq msg (apply 'format code args))
          (setq msg (concat "err: Error \""
                            (prin1-to-string code)
                            "\" does not exist!")))
      (setq msg (apply 'format (cdr err) args)))
    (if where
        (tdy-msg where ": " msg)
      (tdy-msg msg))))

(defun tdy-desc-text (obj &optional full)
  "Determine the description text for 'obj'.  FULL is non-nil if it
should derive the full description text."
  (let* ((attr (if full 't:fdesc 't:sdesc))
         (val (tdy-attr obj attr)))
    (cond ((stringp val)
           val)
          ((functionp val)
           (funcall val obj))
          ((listp val)
           (eval val))
          ((symbolp val)
           (symbol-value val))
          (t nil))))

(defun tdy-describe (&optional thing)
  "Describe an object (or the current place) from the point of view of
the SELF object."
  (let ((full thing))
    (if (not thing)
        (let ((beg (point)))
          (setq thing (tdy-attr tdy-self 't:place))
          (tdy-msg (tdy-noun thing nil t))
          (if (boundp 'font-lock-comment-face)
              (put-text-property beg (point) 'face
                                 font-lock-comment-face))))
    (let ((sym (tdy-sym thing))
          text)
      (if (not (eq tdy-verbosity 'brief))
          (if (and (not full)
                   (get sym 't:seen)
                   (not (eq tdy-verbosity 'super-verbose)))
              (setq text (tdy-desc-text sym))
            (if (not full)
                (tdy-msg))
            (setq text (tdy-desc-text sym t))))
      (if text
          (tdy-msg text))
      (tdy-change sym 't:seen t))))

(defun tdy-move (obj direction)
  "Move the object OBJ from its current place to the place connected to
that place via the direction DIRECTION."
  (let ((current (tdy-attr obj 't:place))
        (dir (downcase direction)))
    (catch 'moved
      (mapcar
       (function
        (lambda (connection)
          (if (equal (car connection) (downcase dir))
              (progn
                (tdy-change obj 't:place (cdr connection))
                (tdy-describe)
                (throw 'moved t)))))
       (get current 't:connections))
      (tdy-err nil 'badmove))))

(defun tdy-find-object (command-words items func &optional strip-the)
  "Given a list of downcased words, see if the initial set of those
words can identify an item within ITEMS.  FUNC is called on each item
within ITEMS to derive its textual representation, or a list of possible
strings to match with.  If a maximal match on any of these strings
succeeds within the entire list (the last maximal match is taken, in the
case of ambiguity), it returns a cons cell, the car of which is the
elements in ITEMS, and the cdr of which is the number of words that were
used from command-words to find the solution -- or 'nil' if no object
was found.  The definite article 'the' is consumed exactly once at the
beginning, if it occurs and STRIP-THE is non-nil."
  (let ((words command-words))
    (if (and strip-the
             (equal (car words) "the"))
        (setq words (cdr words)))
    (let (longest wcount)
      (mapcar
       (function
        (lambda (item)
          (let ((elems (funcall func item)))
            (if (stringp elems)
                (setq elems (list elems)))
            (if (listp elems)
                (mapcar
                 (function
                  (lambda (text)
                    (let* ((twds (split-string (downcase text)))
                           (len (length twds)))
                      (if (<= len (length words))
                          (let ((match t) (index 0))
                            (while (and match
                                        (< index len))
                              (if (not (equal (nth index words)
                                              (nth index twds)))
                                  (setq match nil)
                                (setq index (1+ index))))
                            (if (and match
                                     (= index len))
                                (if (or (not longest)
                                        (> len wcount))
                                    (progn
                                      (setq longest item)
                                      (setq wcount len)))))))))
                 elems)))))
        items)
      (if longest
          (cons longest wcount)
        nil))))

(defvar tdy-parse-analyze nil
  "Display parsing analysis information, as an aid to debugging the
grammar/parser.")

(defun tdy-parse (command)
  "Parse a command, with tdy-self assumed as the doer.  This parser is
not by any means poly-linguistic, but it should work with most latin or
germanic languages.  The basic rules are as follows:

 1.  Only present-tense imperative statements are considered.  The
     assumption is that the human user provided the input is commanding
     the 'tdy-self' object to perform the action.

 2.  Four verb types are allowed.  Note that no real distinction is made
     between the object predicative, and a plain adverbial.  We're not
     parsing Shakespeare here!  But in general, this guides the parser
     as to how it breaks down the input which follows:

       'vi   intransitive        no objects
       'va   predicative         no object and a required adverbial
       'vt   monotransitive      one object (direct)
       'vd   ditransitive        two objects (direct and indirect)
       'vc   complex-transitive  one object and a required adverbial

 3.  Any object specified must be a game object, using the recognized
     name for that object.  Antecedents are not currently maintained, so
     pronouns are not allowed.

 4.  Only simple clauses are parsed: nothing fancy; no ellipticals,
     gerundives, participial phrases...  It's pretty barbarian,
     actually, since you're basically reduced to the level of grunting
     out your desires to some inanimate concept living inside the
     electrical circuitry of a machine.  And you thought you were having
     fun!

 5.  No adjectives or adverbs are necessary yet; on the one hand because
     the game writer is responsible for choosing unique object names,
     and on the other hand because since flourishes don't accomplish
     much in an environment like this.

 6.  Finally, adverbials aren't really broken down very much, which can
     be a pain, especially considering the dreaded _prepositional
     phrase_ (evil music in the background).  Oftentimes, there is a
     requisite indirect object hiding in those phrases, and it's all
     left up to the game writer to deal with it (evil laughter)!  Let's
     the games begin!"
  (if (equal (substring command 0 1) ":")
      (tdy-msg (prin1-to-string
                (eval (read (substring command 1)))))

    ;; This is a messy job, since English isn't very accurate, or
    ;; explicit as to the grammatical case of nouns and the tense of
    ;; verbs.  So we're pretty much going to throw everything away
    ;; that's punctuation, and then strictly rely on the parsing of
    ;; objects to let us know what kind of verb we should look for.
    ;;
    ;; NYI: Note that compound verbs which require an intervening space
    ;; must be avoided at the moment.  This parser will trying their
    ;; compound part as an adverbial or predicate.

    (while (string-match "[-.,;:?!()`]" command)
      (setq command (replace-match " " t t command)))

    (if tdy-parse-analyze
        (tdy-msg (format "parser: command is '%s'" command)))

    (let* ((words (split-string (downcase command)))
           (command (tdy-find-object words tdy-grammar 'car)))
      (if (not command)
          (tdy-err nil 'badcmd)
        (let* ((count (length words))
               (index (cdr command))
               (verb  (funcall 'mapconcat 'identity
                               (subseq words 0 index) " "))
               (begin t) result preds objs)
          (while (< index count)
            (let (isprep)
              (setq result
                    (tdy-find-object (nthcdr index words)
                                     tdy-prepositions 'identity))
              (if result
                  (progn
                    (setq isprep (car result))
                    (if tdy-parse-analyze
                        (tdy-msg (format "parser: found preposition '%s'"
                                         isprep)))
                    (setq index (+ index (cdr result)))))

              ;; check the length again, just to be sure
              (if (< index count)
                  (progn
                    (setq result
                          (tdy-find-object (nthcdr index words)
                                           tdy-objects 'tdy-title t))
                    (if result
                        (progn
                          (if tdy-parse-analyze
                              (tdy-msg (format "parser: found object '%s'"
                                               (tdy-title (car result)))))
                          (if isprep
                              (setq preds
                                    (cons (cons isprep (car result)) preds))
                            (setq objs (cons (car result) objs)))
                          (setq index (+ index (cdr result))))
                      (if tdy-parse-analyze
                          (tdy-msg (format "parser: did not find an object!")))
                      (setq preds
                            (cons (nth index words) preds))
                      (setq index (1+ index)))))))

          ;; now count the objects.  This will determine what kind of verb
          ;; we look for.

          (let ((objcnt (length objs))
                vtype)
            (cond ((= objcnt 0)
                   (setq vtype (if preds 'vp 'vi)))
                  ((= objcnt 1)
                   (setq vtype (if preds 'vc 'vt)))
                  ((>= objcnt 2)
                   (setq vtype 'vd)))
            (if tdy-parse-analyze
                (progn
                  (tdy-msg (format "parser: verb type is '%s'"
                                   (prin1-to-string vtype)))
                  (tdy-msg (format "parser: verb is '%s'" verb))))
            (catch 'handled
              (mapcar
               (function
                (lambda (rule)
                  (let ((words (car rule)))
                    (if (and (eq (cadr rule) vtype)
                             (tdy-match 'equal verb words))
                        (progn
                          (if tdy-parse-analyze
                              (tdy-msg (format "parser: found action for '%s'"
                                               verb)))
                          (cond ((eq vtype 'vi)
                                 (funcall (caddr rule)))
                                ((eq vtype 'vp)
                                 (funcall (caddr rule) preds))
                                ((eq vtype 'vt)
                                 (funcall (caddr rule) (car objs)))
                                ((eq vtype 'vd)
                                 (funcall (caddr rule)
                                          (car objs) (cadr objs)))
                                ((eq vtype 'vc)
                                 (funcall (caddr rule)
                                          (car objs) preds))
                                (t
                                 (error "unknown verb type!")))
                          (throw 'handled t))))))
               tdy-grammar)
              (tdy-err nil 'badcmd))))))))

(defun tdy-prompt ()
  "Output a prompt to the sure, since we're expecting input."
  (setq tdy-current-place
        (format "<%s>" (tdy-title (tdy-location tdy-self))))
  (let ((here (point)))
    (beginning-of-line)
    (if (< (point) here)
        (delete-region (point) here)))
  (insert "> ")
  (goto-char (point-max))
  (recenter -1))

(defun tdy-input ()
  "Get a command from the user after telling them what's going on."
  (interactive "*")
  (beginning-of-line)
  (let ((beg (point)))
    (end-of-line)
    (let ((cmd (buffer-substring-no-properties beg (point))))
      (while (string-match "^> " cmd)
        (setq cmd (replace-match "" nil t cmd)))
      (if (= (length cmd) 0)
          (insert "\n")
        (tdy-parse cmd))
      (if tdy-self
          (tdy-prompt)                ; prompt for next command
        (tdy-quit)
        (kill-buffer "*tdy*")))))

(defun tdy-mode ()
  "Major mode for running tdy."
  (interactive)
  (text-mode)
  (make-local-variable 'scroll-step)
  (setq scroll-step 2)
  (setq tdy-mode-map (make-sparse-keymap))
  (define-key tdy-mode-map "\r" 'tdy-input)
  (use-local-map tdy-mode-map)
  (setq major-mode 'tdy-mode)
  (setq mode-name "TDY"))

(defvar tragedy-version "1.0"
  "First release of the TRAGEDY engine.")

(defun tragedy (file)
  "Let the tragedy begin!  Leap into the past, and the world of text
adventures that beguiled the wits, and fired the imagination.  Don't let
them fool you: such colorful thinking might leave you intractable to
ordinary life: so beware!  And enter with such cautious frame of mind
as is begotten by our later generations..."
  (interactive "fFile containing tragedy: ")

  ;; just in case something didn't get cleaned up from last time...
  (if (or tdy-self
          tdy-objects
          tdy-places
          tdy-connections
          tdy-events)
      (tdy-quit))

  (if (load-file file)
      (progn
        ;; switch to the game buffer...
        (switch-to-buffer "*tdy*")
        (delete-region (point-min) (point-max))
        (tdy-mode)

        ;; in case the user kills the buffer, rather than entering
        ;; 'quit', we'll clean things up anyway.
        (make-local-hook 'kill-buffer-hook)
        (add-hook 'kill-buffer-hook 'tdy-quit nil t)

        ;; always display the current location in the mode line
        (make-local-variable 'global-mode-string)
        (setq global-mode-string
              (append global-mode-string
                      '("  " tdy-current-place)))

        ;; if initialization succeeds, then let the games begin!
        ;; Otherwise, clean up whatever memory had been taken, but leave
        ;; the error messages visible.
        (if (not (tdy-init))
            (tdy-quit)
          (if tdy-intro (tdy-msg tdy-intro))
          (tdy-describe)
          (tdy-prompt)))))

(provide 'tragedy)

;; tragedy.el ends here
.

