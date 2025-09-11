;; Artisan Portfolio Showcase System
;; Enables artisans to create curated portfolios of their best work

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u200))
(define-constant ERR-NOT-FOUND (err u201))
(define-constant ERR-ALREADY-EXISTS (err u202))
(define-constant ERR-INVALID-AMOUNT (err u203))
(define-constant ERR-PORTFOLIO-FULL (err u204))
(define-constant ERR-INVALID-CATEGORY (err u205))

;; Data variables
(define-data-var next-portfolio-id uint u1)
(define-data-var next-showcase-item-id uint u1)

;; Portfolio main data
(define-map artisan-portfolios
  uint ;; artisan-id
  {
    portfolio-title: (string-ascii 100),
    bio: (string-ascii 500),
    specialties: (list 5 (string-ascii 50)),
    years-experience: uint,
    featured-image: (string-ascii 64),
    total-showcase-items: uint,
    portfolio-views: uint,
    is-public: bool,
    created-at: uint,
    updated-at: uint
  }
)

;; Individual showcase items (featured works, achievements, etc.)
(define-map showcase-items
  uint ;; showcase-item-id
  {
    artisan-id: uint,
    item-type: (string-ascii 20), ;; "work", "achievement", "testimonial", "process"
    title: (string-ascii 100),
    description: (string-ascii 300),
    image-hash: (string-ascii 64),
    category: (string-ascii 50),
    year-created: uint,
    is-featured: bool,
    view-count: uint,
    created-at: uint
  }
)

;; Artisan showcase items lookup
(define-map artisan-showcase-items
  uint ;; artisan-id
  (list 20 uint) ;; showcase-item-ids
)

;; Portfolio categories for discovery
(define-map portfolio-categories
  (string-ascii 50) ;; category name
  {
    artisan-count: uint,
    total-views: uint
  }
)

;; Featured portfolios (curated by platform)
(define-map featured-portfolios
  uint ;; position (1-10)
  {
    artisan-id: uint,
    featured-reason: (string-ascii 100),
    featured-at: uint
  }
)

;; Portfolio visitor tracking (basic analytics)
(define-map portfolio-visitors
  {artisan-id: uint, visitor: principal}
  {
    visit-count: uint,
    last-visit: uint
  }
)

;; Read-only functions
(define-read-only (get-artisan-portfolio (artisan-id uint))
  (map-get? artisan-portfolios artisan-id)
)

(define-read-only (get-showcase-item (item-id uint))
  (map-get? showcase-items item-id)
)

(define-read-only (get-artisan-showcase-items (artisan-id uint))
  (default-to (list) (map-get? artisan-showcase-items artisan-id))
)

(define-read-only (get-portfolio-category-stats (category (string-ascii 50)))
  (map-get? portfolio-categories category)
)

(define-read-only (get-featured-portfolio (position uint))
  (map-get? featured-portfolios position)
)

(define-read-only (has-portfolio (artisan-id uint))
  (is-some (map-get? artisan-portfolios artisan-id))
)

;; Public functions - Portfolio Management
(define-public (create-portfolio 
    (artisan-id uint)
    (portfolio-title (string-ascii 100)) 
    (bio (string-ascii 500))
    (specialties (list 5 (string-ascii 50)))
    (years-experience uint)
    (featured-image (string-ascii 64)))
  (let
    (
      (current-block stacks-block-height)
    )
    ;; Basic validation - in real implementation would verify artisan ownership
    (asserts! (> artisan-id u0) ERR-NOT-FOUND)
    (asserts! (> (len portfolio-title) u0) ERR-INVALID-AMOUNT)
    (asserts! (is-none (map-get? artisan-portfolios artisan-id)) ERR-ALREADY-EXISTS)
    
    (map-set artisan-portfolios artisan-id
      {
        portfolio-title: portfolio-title,
        bio: bio,
        specialties: specialties,
        years-experience: years-experience,
        featured-image: featured-image,
        total-showcase-items: u0,
        portfolio-views: u0,
        is-public: true,
        created-at: current-block,
        updated-at: current-block
      }
    )
    
    (map-set artisan-showcase-items artisan-id (list))
    (ok true)
  )
)

(define-public (update-portfolio-info
    (artisan-id uint)
    (portfolio-title (string-ascii 100))
    (bio (string-ascii 500))
    (specialties (list 5 (string-ascii 50)))
    (years-experience uint))
  (let
    (
      (portfolio (unwrap! (map-get? artisan-portfolios artisan-id) ERR-NOT-FOUND))
      (current-block stacks-block-height)
    )
    ;; In real implementation would verify artisan ownership
    (asserts! (> (len portfolio-title) u0) ERR-INVALID-AMOUNT)
    
    (map-set artisan-portfolios artisan-id
      (merge portfolio {
        portfolio-title: portfolio-title,
        bio: bio,
        specialties: specialties,
        years-experience: years-experience,
        updated-at: current-block
      })
    )
    (ok true)
  )
)

(define-public (add-showcase-item
    (artisan-id uint)
    (item-type (string-ascii 20))
    (title (string-ascii 100))
    (description (string-ascii 300))
    (image-hash (string-ascii 64))
    (category (string-ascii 50))
    (year-created uint)
    (is-featured bool))
  (let
    (
      (portfolio (unwrap! (map-get? artisan-portfolios artisan-id) ERR-NOT-FOUND))
      (showcase-item-id (var-get next-showcase-item-id))
      (current-items (get-artisan-showcase-items artisan-id))
      (current-block stacks-block-height)
    )
    ;; In real implementation would verify artisan ownership
    (asserts! (< (len current-items) u20) ERR-PORTFOLIO-FULL)
    (asserts! (> (len title) u0) ERR-INVALID-AMOUNT)
    (asserts! (> (len category) u0) ERR-INVALID-CATEGORY)
    
    (map-set showcase-items showcase-item-id
      {
        artisan-id: artisan-id,
        item-type: item-type,
        title: title,
        description: description,
        image-hash: image-hash,
        category: category,
        year-created: year-created,
        is-featured: is-featured,
        view-count: u0,
        created-at: current-block
      }
    )
    
    ;; Update artisan's showcase items list
    (map-set artisan-showcase-items artisan-id
      (unwrap! (as-max-len? (append current-items showcase-item-id) u20) ERR-PORTFOLIO-FULL)
    )
    
    ;; Update portfolio stats
    (map-set artisan-portfolios artisan-id
      (merge portfolio {
        total-showcase-items: (+ (get total-showcase-items portfolio) u1),
        updated-at: current-block
      })
    )
    
    ;; Update category stats
    (let
      (
        (category-stats (default-to {artisan-count: u0, total-views: u0} 
                         (map-get? portfolio-categories category)))
      )
      (map-set portfolio-categories category
        (merge category-stats {
          artisan-count: (+ (get artisan-count category-stats) u1)
        })
      )
    )
    
    (var-set next-showcase-item-id (+ showcase-item-id u1))
    (ok showcase-item-id)
  )
)

(define-public (remove-showcase-item (artisan-id uint) (item-id uint))
  (let
    (
      (portfolio (unwrap! (map-get? artisan-portfolios artisan-id) ERR-NOT-FOUND))
      (showcase-item (unwrap! (map-get? showcase-items item-id) ERR-NOT-FOUND))
      (current-items (get-artisan-showcase-items artisan-id))
      (current-block stacks-block-height)
    )
    ;; In real implementation would verify artisan ownership
    (asserts! (is-eq (get artisan-id showcase-item) artisan-id) ERR-NOT-AUTHORIZED)
    
    ;; Remove from showcase items
    (map-delete showcase-items item-id)
    
    ;; Update artisan's showcase items list
    (map-set artisan-showcase-items artisan-id
      (filter (is-not-item-id item-id) current-items)
    )
    
    ;; Update portfolio stats
    (map-set artisan-portfolios artisan-id
      (merge portfolio {
        total-showcase-items: (- (get total-showcase-items portfolio) u1),
        updated-at: current-block
      })
    )
    
    (ok true)
  )
)

(define-public (view-portfolio (artisan-id uint))
  (let
    (
      (portfolio (unwrap! (map-get? artisan-portfolios artisan-id) ERR-NOT-FOUND))
      (current-block stacks-block-height)
      (visitor-key {artisan-id: artisan-id, visitor: tx-sender})
      (visitor-data (default-to {visit-count: u0, last-visit: u0} 
                     (map-get? portfolio-visitors visitor-key)))
    )
    (asserts! (get is-public portfolio) ERR-NOT-AUTHORIZED)
    
    ;; Update portfolio view count
    (map-set artisan-portfolios artisan-id
      (merge portfolio {
        portfolio-views: (+ (get portfolio-views portfolio) u1)
      })
    )
    
    ;; Track visitor
    (map-set portfolio-visitors visitor-key
      {
        visit-count: (+ (get visit-count visitor-data) u1),
        last-visit: current-block
      }
    )
    
    (ok portfolio)
  )
)

(define-public (toggle-portfolio-visibility (artisan-id uint))
  (let
    (
      (portfolio (unwrap! (map-get? artisan-portfolios artisan-id) ERR-NOT-FOUND))
      (current-block stacks-block-height)
    )
    ;; In real implementation would verify artisan ownership
    
    (map-set artisan-portfolios artisan-id
      (merge portfolio {
        is-public: (not (get is-public portfolio)),
        updated-at: current-block
      })
    )
    (ok (not (get is-public portfolio)))
  )
)

;; Admin functions
(define-public (feature-portfolio (position uint) (artisan-id uint) (reason (string-ascii 100)))
  (let
    (
      (portfolio (unwrap! (map-get? artisan-portfolios artisan-id) ERR-NOT-FOUND))
      (current-block stacks-block-height)
    )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (and (>= position u1) (<= position u10)) ERR-INVALID-AMOUNT)
    (asserts! (get is-public portfolio) ERR-NOT_AUTHORIZED)
    
    (map-set featured-portfolios position
      {
        artisan-id: artisan-id,
        featured-reason: reason,
        featured-at: current-block
      }
    )
    (ok true)
  )
)

;; Private helper functions
(define-private (is-not-item-id (target-id uint) (item-id uint))
  (not (is-eq item-id target-id))
)

;; Additional read-only functions for discovery
(define-read-only (get-portfolio-stats (artisan-id uint))
  (match (map-get? artisan-portfolios artisan-id)
    portfolio {
      total-items: (get total-showcase-items portfolio),
      total-views: (get portfolio-views portfolio),
      is-public: (get is-public portfolio),
      specialties-count: (len (get specialties portfolio))
    }
    none
  )
)

(define-read-only (search-portfolios-by-category (category (string-ascii 50)))
  (match (map-get? portfolio-categories category)
    stats {
      category: category,
      artisan-count: (get artisan-count stats),
      total-views: (get total-views stats)
    }
    none
  )
)
