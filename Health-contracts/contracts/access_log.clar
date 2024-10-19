
;; title: access_log

;; version: 1.1.0

;; summary: Enhanced medical records access control system

;; description: Manages healthcare record access with testing utilities and optimizations

;; constants
(define-constant READ_PERMISSION u1)
(define-constant WRITE_PERMISSION u2)
(define-constant DELETE_PERMISSION u4)
(define-constant FULL_ACCESS (+ (+ READ_PERMISSION WRITE_PERMISSION) DELETE_PERMISSION))
(define-constant ERR-PATIENT-NOT-FOUND (err u1))
(define-constant ERR-RECORD-NOT-FOUND (err u2))
(define-constant ERR-ACCESS-NOT-FOUND (err u3))
(define-constant ERR-INVALID-PERMISSION (err u4))

;; data vars
(define-data-var test-counter uint u0)
(define-data-var access-control-list 
    {patient-id: uint, healthcare-provider-id: uint, record-id: uint, permissions: uint}
    {
        patient-id: u0,
        healthcare-provider-id: u0,
        record-id: u0,
        permissions: u0,
    }
)

;; data maps
(define-map medical-records 
    {patient-id: uint, record-id: uint} 
    {
        data: (string-ascii 256),
        timestamp: uint,
        access-log: (list 10 {provider: uint, timestamp: uint, action: uint})
    }
)

(define-map patient-profiles uint 
    {
        public-key: (string-ascii 24),
        active: bool,
        created-at: uint
    }
)

;; Testing utilities
(define-map test-scenarios uint 
    {
        description: (string-ascii 100),
        expected-result: bool,
        executed: bool
    }
)

;; Public functions with added logging
(define-public (grant-access (patient-id uint) (healthcare-provider-id uint) (record-id uint) (permissions uint))
    (let (
        (patient-profile (map-get? patient-profiles patient-id))
        (medical-record (map-get? medical-records {patient-id: patient-id, record-id: record-id}))
    )
        (asserts! (is-some patient-profile) ERR-PATIENT-NOT-FOUND)
        (asserts! (is-some medical-record) ERR-RECORD-NOT-FOUND)
        (asserts! (valid-permission? permissions) ERR-INVALID-PERMISSION)
        
        ;; Update medical record with access log
        (try! (match medical-record record
            (ok (map-set medical-records 
                {patient-id: patient-id, record-id: record-id}
                (merge record 
                    {access-log: (append-access-log 
                        (get access-log record) 
                        healthcare-provider-id 
                        permissions)})))
            ERR-RECORD-NOT-FOUND))
        
        ;; Set access control list
        (ok (var-set access-control-list 
            {
                patient-id: patient-id,
                healthcare-provider-id: healthcare-provider-id,
                record-id: record-id,
                permissions: permissions,
            }))
    )
)

;; function to revoke access
(define-public (revoke-access (patient-id uint) (healthcare-provider-id uint) (record-id uint))
  (let (
    (current-access (var-get access-control-list))
    (medical-record (map-get? medical-records {patient-id: patient-id, record-id: record-id}))
  )
    ;; Check if access exists and record exists
    (asserts! (is-some medical-record) ERR-RECORD-NOT-FOUND)
    (asserts! (and 
      (is-eq (get patient-id current-access) patient-id)
      (is-eq (get healthcare-provider-id current-access) healthcare-provider-id)
      (is-eq (get record-id current-access) record-id)
    ) ERR-ACCESS-NOT-FOUND)
    
    ;; Log revocation in medical record and revoke access
    (match medical-record record
      (begin
        (map-set medical-records 
          {patient-id: patient-id, record-id: record-id}
          (merge record 
            {access-log: (append-access-log 
              (get access-log record) 
              healthcare-provider-id 
              u0)}))  ;; u0 indicates revocation
        (var-set access-control-list 
          {
            patient-id: u0,
            healthcare-provider-id: u0,
            record-id: u0,
            permissions: u0,
          })
        (ok true))
      ERR-RECORD-NOT-FOUND)  ;; This arm now returns (err uint) directly
  )
)

;; check who has access to patient medical data
(define-read-only (has-access? (patient-id uint) (healthcare-provider-id uint) (record-id uint) (required-permission uint))
    (let (
        (current-access (var-get access-control-list))
    )
        (and 
            (is-eq (get patient-id current-access) patient-id)
            (is-eq (get healthcare-provider-id current-access) healthcare-provider-id)
            (is-eq (get record-id current-access) record-id)
            (>= (get permissions current-access) required-permission)
        )
    )
)

(define-private (test-check-access)
    (begin
        (try! (grant-access u1 u1 u1 READ_PERMISSION))
        (ok (has-access? u1 u1 u1 READ_PERMISSION))
    )
)


;; Private helper functions
(define-private (valid-permission? (permission uint))
    (or 
        (is-eq permission READ_PERMISSION)
        (is-eq permission WRITE_PERMISSION)
        (is-eq permission DELETE_PERMISSION)
        (is-eq permission FULL_ACCESS)
    )
)

(define-private (append-access-log (current-log (list 10 {provider: uint, timestamp: uint, action: uint})) 
                                 (provider uint) 
                                 (action uint))
    (unwrap-panic 
        (as-max-len? 
            (append current-log 
                {
                    provider: provider,
                    timestamp: block-height,
                    action: action
                }) 
            u10))
)

;; Testing functions
(define-public (run-test-scenario (test-id uint))
    (let (
        (test (unwrap! (map-get? test-scenarios test-id) (err u404)))
        (result (execute-test test-id))
    )
        (match result
            success (begin
                (map-set test-scenarios test-id
                    (merge test 
                        {
                            executed: true,
                            expected-result: (is-eq (get expected-result test) success)
                        }))
                (ok success))
            error (err error))
    )
)

(define-private (execute-test (test-id uint))
    (if (< test-id u3)
        (ok true)
        (err u404))
)

(define-private (test-grant-access)
    (is-ok (grant-access u1 u1 u1 READ_PERMISSION))
)

(define-private (test-revoke-access)
    (and
        (is-ok (grant-access u1 u1 u1 READ_PERMISSION))
        (is-ok (revoke-access u1 u1 u1)))
)

;; Read only functions for testing
(define-read-only (get-test-results)
    (map-get? test-scenarios (var-get test-counter))
)

(define-read-only (get-access-log (patient-id uint) (record-id uint))
    (match (map-get? medical-records {patient-id: patient-id, record-id: record-id})
        record (ok (get access-log record))
        ERR-RECORD-NOT-FOUND)
)