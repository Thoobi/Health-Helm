
;; form-storage.clar
;; Storage contract for form data submissions and management
;; This contract handles the creation, retrieval, and updating of form submissions
;; while maintaining proper access control and data validation.

;; =================================
;; Constants and Error Definitions
;; =================================

;; Error codes for form submission and retrieval
(define-constant ERR-NOT-FOUND (err u404))        ;; Resource not found
(define-constant ERR-UNAUTHORIZED (err u401))      ;; User not authorized for action
(define-constant ERR-INVALID-INPUT (err u400))     ;; Invalid input parameters
(define-constant ERR-NAME-TOO-SHORT (err u1000))   ;; Name field is empty or too short
(define-constant ERR-EMAIL-INVALID (err u1001))    ;; Email format is invalid
(define-constant ERR-MESSAGE-TOO-SHORT (err u1002));; Message is empty or too short
(define-constant ERR-SUBMISSION-LIMIT (err u1003)) ;; Maximum submission limit reached
(define-constant ERR-PHONE-INVALID (err u1004))    ;; Phone number format is invalid
(define-constant ERR-UPDATE-FAILED (err u1005))    ;; Failed to update submission

;; =================================
;; Data Variables and Maps
;; =================================

;; Map to store form submissions
;; Keys: submission-id (uint)
;; Values: submission details including sender information
(define-map form-submissions
    { submission-id: uint }
    {
        name: (string-utf8 100),      ;; Name of the submitter
        email: (string-utf8 100),     ;; Email address for contact
        phone: (string-utf8 20),      ;; Optional phone number
        message: (string-utf8 500),   ;; Main message content
        timestamp: uint,              ;; Block timestamp of submission
        sender: principal            ;; Principal who submitted the form
    }
)

;; Counter to generate unique submission IDs
(define-data-var submission-counter uint u0)

;; =================================
;; Public Functions
;; =================================

;; Submit a new form entry
;; @param name: Name of the submitter (required)
;; @param email: Email address for contact (required)
;; @param phone: Phone number (optional)
;; @param message: Main message content (required)
;; @returns (response uint uint) - Success returns submission ID, failure returns error code
(define-public (submit-form 
    (name (string-utf8 100))
    (email (string-utf8 100))
    (phone (string-utf8 20))
    (message (string-utf8 500)))
    
    (let
        (
            (submission-id (var-get submission-counter))
            (timestamp (unwrap-panic (get-block-info? time u0)))
        )
        ;; Input validation
        (asserts! (> (len name) u0) ERR-NAME-TOO-SHORT)
        (asserts! (> (len email) u0) ERR-EMAIL-INVALID)
        (asserts! (> (len message) u0) ERR-MESSAGE-TOO-SHORT)

        ;; Store the submission
        (map-set form-submissions
            { submission-id: submission-id }
            {
                name: name,
                email: email,
                phone: phone,
                message: message,
                timestamp: timestamp,
                sender: tx-sender
            }
        )

        ;; Increment the counter
        (var-set submission-counter (+ submission-id u1))

        ;; Return success with submission ID
        (ok submission-id)
    )
)

;; Retrieve a specific submission by ID
;; @param submission-id: The unique identifier of the submission
;; @returns (response {submission-data} uint) - Success returns submission details, failure returns error code
(define-read-only (get-submission (submission-id uint))
    (match (map-get? form-submissions { submission-id: submission-id })
        submission (ok submission)  ;; Return the submission if found
        ERR-NOT-FOUND              ;; Return error if not found
    )
)

;; Get total number of submissions
;; @returns (response uint uint) - Success returns submission count
(define-read-only (get-submission-count)
    (ok (var-get submission-counter))
)

;; Update an existing submission
;; @param submission-id: The unique identifier of the submission to update
;; @param name: Updated name
;; @param email: Updated email
;; @param phone: Updated phone
;; @param message: Updated message
;; @returns (response bool uint) - Success returns true, failure returns error code
(define-public (update-submission 
    (submission-id uint)
    (name (string-utf8 100))
    (email (string-utf8 100))
    (phone (string-utf8 20))
    (message (string-utf8 500)))
    
    (let
        (
            (submission (unwrap! (map-get? form-submissions { submission-id: submission-id }) ERR-NOT-FOUND))
        )
        ;; Verify ownership
        (asserts! (is-eq tx-sender (get sender submission)) ERR-UNAUTHORIZED)
        
        ;; Input validation
        (asserts! (> (len name) u0) ERR-NAME-TOO-SHORT)
        (asserts! (> (len email) u0) ERR-EMAIL-INVALID)
        (asserts! (> (len message) u0) ERR-MESSAGE-TOO-SHORT)

        ;; Update the submission
        (map-set form-submissions
            { submission-id: submission-id }
            {
                name: name,
                email: email,
                phone: phone,
                message: message,
                timestamp: (get timestamp submission),
                sender: tx-sender
            }
        )

        (ok true)
    )
)

;; =================================
;; Read-only Helper Functions
;; =================================

;; Get the next available submission ID
;; @returns uint - The next submission ID to be used
(define-read-only (get-next-submission-id)
    (var-get submission-counter)
)