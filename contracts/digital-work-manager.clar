;; Digital Homework Club Manager
;; A comprehensive after-school study group coordination system with parent communication

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u1))
(define-constant ERR_NOT_FOUND (err u2))
(define-constant ERR_ALREADY_EXISTS (err u3))
(define-constant ERR_INVALID_SESSION (err u4))
(define-constant ERR_SESSION_FULL (err u5))
(define-constant ERR_INVALID_TIME (err u6))
(define-constant ERR_ALREADY_ENROLLED (err u7))
(define-constant ERR_NOT_ENROLLED (err u8))

;; Data Variables
(define-data-var next-session-id uint u1)
(define-data-var next-student-id uint u1)
(define-data-var next-tutor-id uint u1)

;; Data Maps
(define-map sessions
  { session-id: uint }
  {
    title: (string-ascii 64),
    subject: (string-ascii 32),
    tutor-id: uint,
    start-block: uint,
    duration-blocks: uint,
    max-students: uint,
    enrolled-count: uint,
    location: (string-ascii 64),
    status: (string-ascii 16), ;; "scheduled", "active", "completed", "cancelled"
    created-at: uint
  }
)

(define-map students
  { student-id: uint }
  {
    name: (string-ascii 64),
    grade: uint,
    parent-principal: principal,
    emergency-contact: (string-ascii 128),
    created-at: uint,
    active: bool
  }
)

(define-map tutors
  { tutor-id: uint }
  {
    name: (string-ascii 64),
    subjects: (list 5 (string-ascii 32)),
    hourly-rate: uint,
    rating: uint, ;; out of 100
    total-sessions: uint,
    active: bool,
    created-at: uint
  }
)

(define-map student-enrollments
  { session-id: uint, student-id: uint }
  {
    enrolled-at: uint,
    attendance: (string-ascii 16), ;; "pending", "present", "absent", "excused"
    progress-notes: (string-ascii 256)
  }
)

(define-map session-progress
  { session-id: uint }
  {
    completion-percentage: uint,
    homework-completed: uint,
    homework-assigned: uint,
    notes: (string-ascii 512),
    updated-at: uint
  }
)

(define-map parent-communications
  { session-id: uint, student-id: uint }
  {
    message: (string-ascii 512),
    sender: principal,
    sent-at: uint,
    message-type: (string-ascii 16) ;; "progress", "attendance", "assignment", "general"
  }
)

;; Principal to student/tutor mappings
(define-map principal-to-student principal uint)
(define-map principal-to-tutor principal uint)

;; Read-only functions
(define-read-only (get-session (session-id uint))
  (map-get? sessions { session-id: session-id })
)

(define-read-only (get-student (student-id uint))
  (map-get? students { student-id: student-id })
)

(define-read-only (get-tutor (tutor-id uint))
  (map-get? tutors { tutor-id: tutor-id })
)

(define-read-only (get-student-enrollment (session-id uint) (student-id uint))
  (map-get? student-enrollments { session-id: session-id, student-id: student-id })
)

(define-read-only (get-session-progress (session-id uint))
  (map-get? session-progress { session-id: session-id })
)

(define-read-only (get-student-by-principal (parent principal))
  (map-get? principal-to-student parent)
)

(define-read-only (get-tutor-by-principal (tutor principal))
  (map-get? principal-to-tutor tutor)
)

(define-read-only (is-session-available (session-id uint))
  (match (get-session session-id)
    session-data (< (get enrolled-count session-data) (get max-students session-data))
    false
  )
)

(define-read-only (get-current-block)
  stacks-block-height
)

;; Administrative functions
(define-public (register-student (name (string-ascii 64)) (grade uint) (parent-principal principal) (emergency-contact (string-ascii 128)))
  (let
    (
      (student-id (var-get next-student-id))
      (current-block (get-current-block))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)

    (map-set students
      { student-id: student-id }
      {
        name: name,
        grade: grade,
        parent-principal: parent-principal,
        emergency-contact: emergency-contact,
        created-at: current-block,
        active: true
      }
    )

    (map-set principal-to-student parent-principal student-id)
    (var-set next-student-id (+ student-id u1))

    (ok student-id)
  )
)

(define-public (register-tutor (name (string-ascii 64)) (subjects (list 5 (string-ascii 32))) (hourly-rate uint) (tutor-principal principal))
  (let
    (
      (tutor-id (var-get next-tutor-id))
      (current-block (get-current-block))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)

    (map-set tutors
      { tutor-id: tutor-id }
      {
        name: name,
        subjects: subjects,
        hourly-rate: hourly-rate,
        rating: u80,
        total-sessions: u0,
        active: true,
        created-at: current-block
      }
    )

    (map-set principal-to-tutor tutor-principal tutor-id)
    (var-set next-tutor-id (+ tutor-id u1))

    (ok tutor-id)
  )
)

;; Session management functions
(define-public (create-session
    (title (string-ascii 64))
    (subject (string-ascii 32))
    (tutor-id uint)
    (start-block uint)
    (duration-blocks uint)
    (max-students uint)
    (location (string-ascii 64)))
  (let
    (
      (session-id (var-get next-session-id))
      (current-block (get-current-block))
    )
    (asserts! (or (is-eq tx-sender CONTRACT_OWNER)
                  (is-some (map-get? principal-to-tutor tx-sender))) ERR_NOT_AUTHORIZED)
    (asserts! (> start-block current-block) ERR_INVALID_TIME)
    (asserts! (is-some (get-tutor tutor-id)) ERR_NOT_FOUND)

    (map-set sessions
      { session-id: session-id }
      {
        title: title,
        subject: subject,
        tutor-id: tutor-id,
        start-block: start-block,
        duration-blocks: duration-blocks,
        max-students: max-students,
        enrolled-count: u0,
        location: location,
        status: "scheduled",
        created-at: current-block
      }
    )

    (var-set next-session-id (+ session-id u1))
    (ok session-id)
  )
)

(define-public (enroll-student (session-id uint) (student-id uint))
  (let
    (
      (session-data (unwrap! (get-session session-id) ERR_NOT_FOUND))
      (student-data (unwrap! (get-student student-id) ERR_NOT_FOUND))
      (current-block (get-current-block))
    )
    ;; Check if parent or admin is enrolling
    (asserts! (or
      (is-eq tx-sender CONTRACT_OWNER)
      (is-eq tx-sender (get parent-principal student-data))) ERR_NOT_AUTHORIZED)

    ;; Check if session is not full
    (asserts! (< (get enrolled-count session-data) (get max-students session-data)) ERR_SESSION_FULL)

    ;; Check if student is not already enrolled
    (asserts! (is-none (get-student-enrollment session-id student-id)) ERR_ALREADY_ENROLLED)

    ;; Enroll student
    (map-set student-enrollments
      { session-id: session-id, student-id: student-id }
      {
        enrolled-at: current-block,
        attendance: "pending",
        progress-notes: ""
      }
    )

    ;; Update enrollment count
    (map-set sessions
      { session-id: session-id }
      (merge session-data { enrolled-count: (+ (get enrolled-count session-data) u1) })
    )

    (ok true)
  )
)

(define-public (mark-attendance (session-id uint) (student-id uint) (attendance (string-ascii 16)))
  (let
    (
      (enrollment-data (unwrap! (get-student-enrollment session-id student-id) ERR_NOT_FOUND))
      (session-data (unwrap! (get-session session-id) ERR_NOT_FOUND))
    )
    ;; Only tutor assigned to session or admin can mark attendance
    (asserts! (or
      (is-eq tx-sender CONTRACT_OWNER)
      (and
        (is-some (map-get? principal-to-tutor tx-sender))
        (is-eq (get tutor-id session-data)
               (unwrap! (map-get? principal-to-tutor tx-sender) ERR_NOT_AUTHORIZED)))) ERR_NOT_AUTHORIZED)

    (map-set student-enrollments
      { session-id: session-id, student-id: student-id }
      (merge enrollment-data { attendance: attendance })
    )

    (ok true)
  )
)

;; Progress tracking functions
(define-public (update-session-progress
    (session-id uint)
    (completion-percentage uint)
    (homework-completed uint)
    (homework-assigned uint)
    (notes (string-ascii 512)))
  (let
    (
      (session-data (unwrap! (get-session session-id) ERR_NOT_FOUND))
      (current-block (get-current-block))
    )
    ;; Only assigned tutor or admin can update progress
    (asserts! (or
      (is-eq tx-sender CONTRACT_OWNER)
      (and
        (is-some (map-get? principal-to-tutor tx-sender))
        (is-eq (get tutor-id session-data)
               (unwrap! (map-get? principal-to-tutor tx-sender) ERR_NOT_AUTHORIZED)))) ERR_NOT_AUTHORIZED)

    (map-set session-progress
      { session-id: session-id }
      {
        completion-percentage: completion-percentage,
        homework-completed: homework-completed,
        homework-assigned: homework-assigned,
        notes: notes,
        updated-at: current-block
      }
    )

    (ok true)
  )
)

(define-public (add-student-progress-notes (session-id uint) (student-id uint) (notes (string-ascii 256)))
  (let
    (
      (enrollment-data (unwrap! (get-student-enrollment session-id student-id) ERR_NOT_FOUND))
      (session-data (unwrap! (get-session session-id) ERR_NOT_FOUND))
    )
    ;; Only assigned tutor or admin can add progress notes
    (asserts! (or
      (is-eq tx-sender CONTRACT_OWNER)
      (and
        (is-some (map-get? principal-to-tutor tx-sender))
        (is-eq (get tutor-id session-data)
               (unwrap! (map-get? principal-to-tutor tx-sender) ERR_NOT_AUTHORIZED)))) ERR_NOT_AUTHORIZED)

    (map-set student-enrollments
      { session-id: session-id, student-id: student-id }
      (merge enrollment-data { progress-notes: notes })
    )

    (ok true)
  )
)

;; Parent communication functions
(define-public (send-parent-message
    (session-id uint)
    (student-id uint)
    (message (string-ascii 512))
    (message-type (string-ascii 16)))
  (let
    (
      (session-data (unwrap! (get-session session-id) ERR_NOT_FOUND))
      (student-data (unwrap! (get-student student-id) ERR_NOT_FOUND))
      (current-block (get-current-block))
    )
    ;; Only assigned tutor, admin, or parent can send messages
    (asserts! (or
      (is-eq tx-sender CONTRACT_OWNER)
      (is-eq tx-sender (get parent-principal student-data))
      (and
        (is-some (map-get? principal-to-tutor tx-sender))
        (is-eq (get tutor-id session-data)
               (unwrap! (map-get? principal-to-tutor tx-sender) ERR_NOT_AUTHORIZED)))) ERR_NOT_AUTHORIZED)

    (map-set parent-communications
      { session-id: session-id, student-id: student-id }
      {
        message: message,
        sender: tx-sender,
        sent-at: current-block,
        message-type: message-type
      }
    )

    (ok true)
  )
)

;; Session status management
(define-public (update-session-status (session-id uint) (new-status (string-ascii 16)))
  (let
    (
      (session-data (unwrap! (get-session session-id) ERR_NOT_FOUND))
    )
    ;; Only assigned tutor or admin can update session status
    (asserts! (or
      (is-eq tx-sender CONTRACT_OWNER)
      (and
        (is-some (map-get? principal-to-tutor tx-sender))
        (is-eq (get tutor-id session-data)
               (unwrap! (map-get? principal-to-tutor tx-sender) ERR_NOT_AUTHORIZED)))) ERR_NOT_AUTHORIZED)

    (map-set sessions
      { session-id: session-id }
      (merge session-data { status: new-status })
    )

    ;; If completing session, increment tutor's total sessions
    (if (is-eq new-status "completed")
      (let
        (
          (tutor-data (unwrap! (get-tutor (get tutor-id session-data)) ERR_NOT_FOUND))
        )
        (map-set tutors
          { tutor-id: (get tutor-id session-data) }
          (merge tutor-data { total-sessions: (+ (get total-sessions tutor-data) u1) })
        )
        (ok true)
      )
      (ok true)
    )
  )
)

;; Utility functions
(define-public (update-tutor-rating (tutor-id uint) (new-rating uint))
  (let
    (
      (tutor-data (unwrap! (get-tutor tutor-id) ERR_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (<= new-rating u100) ERR_INVALID_SESSION)

    (map-set tutors
      { tutor-id: tutor-id }
      (merge tutor-data { rating: new-rating })
    )

    (ok true)
  )
)

(define-public (deactivate-student (student-id uint))
  (let
    (
      (student-data (unwrap! (get-student student-id) ERR_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)

    (map-set students
      { student-id: student-id }
      (merge student-data { active: false })
    )

    (ok true)
  )
)

(define-public (deactivate-tutor (tutor-id uint))
  (let
    (
      (tutor-data (unwrap! (get-tutor tutor-id) ERR_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)

    (map-set tutors
      { tutor-id: tutor-id }
      (merge tutor-data { active: false })
    )

    (ok true)
  )
)
