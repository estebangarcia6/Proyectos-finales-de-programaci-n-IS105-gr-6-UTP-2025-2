#lang racket
(require 2htdp/universe
         2htdp/image)

;constante
(define WIDTH 800)
(define HEIGHT 600)
(define FLOOR-Y 500)
(define GRAVITY 1)         
(define JUMP-VY -20)
(define WALK-DX 8)
(define TICK-SEC (/ 1 60))

;Imagenes
(define portada-img (bitmap "Imagenportada.png"))
(define fondo-img   (bitmap "fondo.png"))

; Cargar imagen de la cebollita
(define cebollita-img 
  (if (file-exists? "cebollita.png")
      (scale 0.8 (bitmap "cebollita.png"))
      (overlay
       (circle 22 "solid" "purple")
       (circle 25 "solid" "pink")
       (circle 30 "solid" "white"))))

; Cargar imagen para la montañita/plataforma
(define montanita-img 
  (if (file-exists? "montañita.png")
      (bitmap "montañita.png")
      (overlay
       (ellipse 150 60 "solid" (color 139 69 19))
       (ellipse 140 50 "solid" (color 160 82 45))
       (ellipse 130 40 "solid" (color 205 133 63)))))

; Cargar imagen para la estrellita
(define estrellita-img 
  (if (file-exists? "estrellita.png")
      (scale 0.5 (bitmap "estrellita.png"))
      (scale 0.6 (star 30 "solid" "gold"))))

; Cargar imagen para el tomate enemigo
(define tomate-img 
  (if (file-exists? "tomate.png")
      (scale 0.7 (bitmap "tomate.png"))
      (overlay
       (circle 20 "solid" "red")
       (circle 16 "solid" "darkred")
       (ellipse 10 5 "solid" "green")
       (ellipse 6 3 "solid" "lightgreen"))))

; Dimensiones
(define MONTANITA-WIDTH (image-width montanita-img))
(define MONTANITA-HEIGHT (image-height montanita-img))
(define ESTRELLITA-SIZE (image-width estrellita-img))
(define TOMATE-SIZE (image-width tomate-img))
(define CEBOLLITA-WIDTH (image-width cebollita-img))
(define CEBOLLITA-HEIGHT (image-height cebollita-img))

; ============================================================
; POSICIONES MEJORADAS PARA NIVEL 3 - PIRÁMIDE
; ============================================================

; Nivel 1 (sin cambios)
(define MONTANITA-X (/ WIDTH 2))
(define MONTANITA-Y (- FLOOR-Y (/ MONTANITA-HEIGHT 2)))
(define CEBOLLITA-INICIAL-X 50)
(define ESTRELLITA-X 750)
(define ESTRELLITA-Y (- FLOOR-Y 120))

; Nivel 2 (sin cambios)
(define MONTANITA1-X 250)
(define MONTANITA1-Y (- FLOOR-Y (/ MONTANITA-HEIGHT 2)))
(define MONTANITA2-X 600)
(define MONTANITA2-Y (- FLOOR-Y (/ MONTANITA-HEIGHT 2)))
(define ESTRELLITA2-X 750)
(define ESTRELLITA2-Y (- FLOOR-Y 120))
(define TOMATE-INICIAL-X MONTANITA1-X)
(define TOMATE-MIN-X (- MONTANITA1-X 60))
(define TOMATE-MAX-X (+ MONTANITA1-X 60))
(define TOMATE-Y (- MONTANITA1-Y 70))

; ============================================================
; NUEVO NIVEL 3 - PIRÁMIDE CON MONTAÑAS SEPARADAS
; ============================================================

; Montaña izquierda en el suelo
(define MONTANITA3-LEFT-X 200)
(define MONTANITA3-LEFT-Y (- FLOOR-Y (/ MONTANITA-HEIGHT 2)))

; Montaña derecha en el suelo
(define MONTANITA3-RIGHT-X 600)
(define MONTANITA3-RIGHT-Y (- FLOOR-Y (/ MONTANITA-HEIGHT 2)))

; Montaña flotante en el centro arriba
(define MONTANITA3-CENTER-X 400)  ; Centro entre las dos montañas
(define MONTANITA3-CENTER-Y (- FLOOR-Y 200))  ; 200px más arriba que el suelo

; Estrella en la parte superior derecha
(define ESTRELLITA3-X 750)
(define ESTRELLITA3-Y (- FLOOR-Y 180))  ; Un poco más arriba que la montaña central

; Tomate en la montaña izquierda (se mueve sobre ella)
(define TOMATE3-LEFT-X MONTANITA3-LEFT-X)
(define TOMATE3-LEFT-MIN-X (- MONTANITA3-LEFT-X 60))
(define TOMATE3-LEFT-MAX-X (+ MONTANITA3-LEFT-X 60))
(define TOMATE3-LEFT-Y (- MONTANITA3-LEFT-Y 70))  ; 70px sobre la montaña izquierda

; Tomate en la montaña flotante (se mueve sobre ella)
(define TOMATE3-CENTER-X MONTANITA3-CENTER-X)
(define TOMATE3-CENTER-MIN-X (- MONTANITA3-CENTER-X 60))
(define TOMATE3-CENTER-MAX-X (+ MONTANITA3-CENTER-X 60))
(define TOMATE3-CENTER-Y (- MONTANITA3-CENTER-Y 70))  ; 70px sobre la montaña central

; ============================================================
; FUNCIONES DE COLISIÓN (sin cambios)
; ============================================================

(define (check-platform-collision player-x player-y player-width player-height platform-x platform-y platform-w platform-h)
  (let* ([player-bottom (+ player-y (/ player-height 2))]
         [player-top (- player-y (/ player-height 2))]
         [player-left (- player-x (/ player-width 2))]
         [player-right (+ player-x (/ player-width 2))]
         [platform-top (- platform-y (/ platform-h 2))]
         [platform-bottom (+ platform-y (/ platform-h 2))]
         [platform-left (- platform-x (/ platform-w 2))]
         [platform-right (+ platform-x (/ platform-w 2))])
    
    (and (> (- player-right 8) platform-left)
         (< (+ player-left 8) platform-right)
         (> player-bottom platform-top)
         (< player-bottom (+ platform-top 15))
         #t)))

(define (check-on-top-of-platform? player-x player-y player-width player-height platform-x platform-y platform-w platform-h)
  (let* ([player-bottom (+ player-y (/ player-height 2))]
         [player-left (- player-x (/ player-width 2))]
         [player-right (+ player-x (/ player-width 2))]
         [platform-top (- platform-y (/ platform-h 2))]
         [platform-left (- platform-x (/ platform-w 2))]
         [platform-right (+ platform-x (/ platform-w 2))])
    
    (and (<= (abs (- player-bottom platform-top)) 2)
         (> (- player-right 10) platform-left)
         (< (+ player-left 10) platform-right))))

(define (check-side-collision player-x player-y player-width player-height platform-x platform-y platform-w platform-h)
  (let* ([player-bottom (+ player-y (/ player-height 2))]
         [player-top (- player-y (/ player-height 2))]
         [player-left (- player-x (/ player-width 2))]
         [player-right (+ player-x (/ player-width 2))]
         [platform-top (- platform-y (/ platform-h 2))]
         [platform-bottom (+ platform-y (/ platform-h 2))]
         [platform-left (- platform-x (/ platform-w 2))]
         [platform-right (+ platform-x (/ platform-w 2))])
    
    (let ([vertical-overlap? (and (< player-top (+ platform-bottom 3))
                                  (> player-bottom (- platform-top 3)))])
      (and vertical-overlap?
           (or (and (< (abs (- player-right platform-left)) 5)
                    (> player-right platform-left))
               (and (< (abs (- player-left platform-right)) 5)
                    (< player-left platform-right)))))))

(define (check-enemy-collision player-x player-y player-width player-height enemy-x enemy-y enemy-w enemy-h)
  (let* ([player-center-x player-x]
         [player-center-y player-y]
         [enemy-center-x enemy-x]
         [enemy-center-y enemy-y]
         [player-radius (* (/ player-width 2) 0.75)]
         [enemy-radius (* (/ enemy-w 2) 0.75)]
         [distance-x (abs (- player-center-x enemy-center-x))]
         [distance-y (abs (- player-center-y enemy-center-y))]
         [sum-radii (+ player-radius enemy-radius)])
    (and (< distance-x sum-radii)
         (< distance-y sum-radii))))

(define (check-star-collision player-x player-y player-width player-height star-x star-y star-size)
  (let* ([player-center-x player-x]
         [player-center-y player-y]
         [star-center-x star-x]
         [star-center-y star-y]
         [player-radius (* (/ player-width 2) 0.8)]
         [star-radius (* (/ star-size 2) 0.8)]
         [distance-x (abs (- player-center-x star-center-x))]
         [distance-y (abs (- player-center-y star-center-y))]
         [sum-radii (+ player-radius star-radius)])
    (and (< distance-x sum-radii)
         (< distance-y sum-radii))))

; ============================================================
; FUNCIONES AUXILIARES (sin cambios)
; ============================================================

(define (background-image img)
  (place-image img (/ WIDTH 2) (/ HEIGHT 2) (empty-scene WIDTH HEIGHT)))

(define (draw-level-button n x y)
  (overlay
   (text (number->string n) 30 "white")
   (rectangle 120 80 "solid" (color 100 100 100 200))
   (rectangle 124 84 "solid" "black")))

(define (inside? click-x click-y btn-x btn-y width height)
  (and (>= click-x (- btn-x (/ width 2)))
       (<= click-x (+ btn-x (/ width 2)))
       (>= click-y (- btn-y (/ height 2)))
       (<= click-y (+ btn-y (/ height 2)))))

(define (draw-failed-level-message)
  (overlay (above (text "NIVEL FALLIDO" 48 "red")
                  (text "Presiona R para reintentar" 24 "darkgray"))
           (rectangle 550 150 "solid" (color 255 255 255 230))))

; ============================================================
; ESTRUCTURA DEL MUNDO (actualizada para nivel 3)
; ============================================================

(define (make-world mode x y vy on-floor? crouched? game-over? level-time level-complete? 
                    tomate-x tomate-dir tomate-center-x tomate-center-dir star-collected? last-key)
  (hash 'mode mode
        'x x
        'y y
        'vy vy
        'on-floor? on-floor?
        'crouched? crouched?
        'game-over? game-over?
        'level-time level-time
        'level-complete? level-complete?
        'tomate-x tomate-x           ; Posición del tomate en montaña izquierda
        'tomate-dir tomate-dir       ; Dirección del tomate izquierdo
        'tomate-center-x tomate-center-x     ; Posición del tomate en montaña central
        'tomate-center-dir tomate-center-dir ; Dirección del tomate central
        'star-collected? star-collected?
        'last-key last-key))

(define initial-world (make-world 'menu CEBOLLITA-INICIAL-X (- FLOOR-Y (/ CEBOLLITA-HEIGHT 2)) 
                                  0 #t #f #f 0 #f 
                                  TOMATE3-LEFT-X 1 TOMATE3-CENTER-X 1 #f "none"))

(define (player-image crouched?)
  (let* ([scale-factor (if crouched? 0.9 1.0)])
    (if (file-exists? "cebollita.png")
        (scale scale-factor cebollita-img)
        (let ([size (if crouched? 25 28)])
          (overlay
           (circle size "solid" "purple")
           (circle (+ size 3) "solid" "pink")
           (circle (+ size 6) "solid" "white"))))))

; ============================================================
; DIBUJO DEL NIVEL 3 ACTUALIZADO
; ============================================================

(define (draw-world w)
  (cond
    ;; ---- MENU INICIAL ----
    [(eq? (hash-ref w 'mode) 'menu)
     (place-image (text "Press SPACE to start" 32 'white)
                  (/ WIDTH 2) 500
                  (place-image (text "SUPER ONION" 64 'yellow)
                               (/ WIDTH 2) 300
                               (background-image portada-img)))]

    ;; ---- MENU DE NIVELES ----
    [(eq? (hash-ref w 'mode) 'menu-niveles)
     (let* ([base (place-image (text "SELECT LEVEL" 48 'black)
                               (/ WIDTH 2) 100
                               (background-image fondo-img))]
            [s1 (place-image (draw-level-button 1 200 300) 200 300 base)]
            [s2 (place-image (draw-level-button 2 400 300) 400 300 s1)]
            [s3 (place-image (draw-level-button 3 600 300) 600 300 s2)]
            [s4 (place-image (text "Click on a level number" 20 'darkgray)
                             (/ WIDTH 2) 500 s3)])
       s4)]

    ;; ---- NIVEL 1 ----
    [(eq? (hash-ref w 'mode) 'nivel1)
     (let* ([x (hash-ref w 'x)]
            [y (hash-ref w 'y)]
            [crouched? (hash-ref w 'crouched?)]
            [game-over? (hash-ref w 'game-over?)]
            [level-time (hash-ref w 'level-time)]
            [level-complete? (hash-ref w 'level-complete?)]
            [star-collected? (hash-ref w 'star-collected?)]
            
            [base-scene (background-image fondo-img)]
            
            [scene-con-montanita (place-image montanita-img MONTANITA-X MONTANITA-Y base-scene)]
            
            [scene-con-estrellita (if (not star-collected?)
                                      (place-image estrellita-img ESTRELLITA-X ESTRELLITA-Y scene-con-montanita)
                                      scene-con-montanita)]
            
            [player (player-image crouched?)]
            [scene-con-player (place-image player x y scene-con-estrellita)]
            
            [time-bg (rectangle 120 30 "solid" (color 0 0 0 150))]
            [scene-con-tiempo (place-image (text (string-append "TIME: " (number->string (inexact->exact (floor level-time)))) 20 'white)
                                           70 30
                                           (place-image time-bg 70 30 scene-con-player))])
       
       (cond
         [level-complete?
          (overlay (overlay (above (text "NIVEL COMPLETADO" 48 "white")
                                   (text "Presiona ESC para salir" 24 "lightgray"))
                            (rectangle 600 200 "solid" (color 173 216 230 220)))
                   scene-con-tiempo)]
         
         [game-over?
          (overlay (draw-failed-level-message) scene-con-tiempo)]
         
         [else scene-con-tiempo]))]

    ;; ---- NIVEL 2 ----
    [(eq? (hash-ref w 'mode) 'nivel2)
     (let* ([x (hash-ref w 'x)]
            [y (hash-ref w 'y)]
            [crouched? (hash-ref w 'crouched?)]
            [game-over? (hash-ref w 'game-over?)]
            [level-time (hash-ref w 'level-time)]
            [level-complete? (hash-ref w 'level-complete?)]
            [tomate-x (hash-ref w 'tomate-x)]
            [tomate-dir (hash-ref w 'tomate-dir)]
            [star-collected? (hash-ref w 'star-collected?)]
            
            [base-scene (background-image fondo-img)]
            
            [scene-con-montanita1 (place-image montanita-img MONTANITA1-X MONTANITA1-Y base-scene)]
            [scene-con-montanita2 (place-image montanita-img MONTANITA2-X MONTANITA2-Y scene-con-montanita1)]
            
            [scene-con-tomate (place-image tomate-img tomate-x TOMATE-Y scene-con-montanita2)]
            
            [scene-con-estrellita (if (not star-collected?)
                                      (place-image estrellita-img ESTRELLITA2-X ESTRELLITA2-Y scene-con-tomate)
                                      scene-con-tomate)]
            
            [player (player-image crouched?)]
            [scene-con-player (place-image player x y scene-con-estrellita)]
            
            [time-bg (rectangle 120 30 "solid" (color 0 0 0 150))]
            [scene-con-tiempo (place-image (text (string-append "TIME: " (number->string (inexact->exact (floor level-time)))) 20 'white)
                                           70 30
                                           (place-image time-bg 70 30 scene-con-player))])
       
       (cond
         [level-complete?
          (overlay (overlay (above (text "NIVEL COMPLETADO" 48 "white")
                                   (text "Presiona ESC para salir" 24 "lightgray"))
                            (rectangle 600 200 "solid" (color 173 216 230 220)))
                   scene-con-tiempo)]
         
         [game-over?
          (overlay (draw-failed-level-message) scene-con-tiempo)]
         
         [else scene-con-tiempo]))]

    ;; ---- NIVEL 3 - PIRÁMIDE MEJORADA ----
    [(eq? (hash-ref w 'mode) 'nivel3)
     (let* ([x (hash-ref w 'x)]
            [y (hash-ref w 'y)]
            [crouched? (hash-ref w 'crouched?)]
            [game-over? (hash-ref w 'game-over?)]
            [level-time (hash-ref w 'level-time)]
            [level-complete? (hash-ref w 'level-complete?)]
            [tomate-left-x (hash-ref w 'tomate-x)]
            [tomate-left-dir (hash-ref w 'tomate-dir)]
            [tomate-center-x (hash-ref w 'tomate-center-x)]
            [tomate-center-dir (hash-ref w 'tomate-center-dir)]
            [star-collected? (hash-ref w 'star-collected?)]
            
            [base-scene (background-image fondo-img)]
            
            ; Añadir nubes decorativas para el nivel difícil
            [nube (circle 25 "solid" (color 255 255 255 180))]
            [scene-con-nube1 (place-image nube 150 150 base-scene)]
            [scene-con-nube2 (place-image nube 650 120 scene-con-nube1)]
            
            ; Montañas en forma de pirámide
            [scene-con-montanita-left (place-image montanita-img MONTANITA3-LEFT-X MONTANITA3-LEFT-Y scene-con-nube2)]
            [scene-con-montanita-right (place-image montanita-img MONTANITA3-RIGHT-X MONTANITA3-RIGHT-Y scene-con-montanita-left)]
            [scene-con-montanita-center (place-image montanita-img MONTANITA3-CENTER-X MONTANITA3-CENTER-Y scene-con-montanita-right)]
            
            ; Tomate en montaña izquierda
            [scene-con-tomate-left (place-image tomate-img tomate-left-x TOMATE3-LEFT-Y scene-con-montanita-center)]
            
            ; Tomate en montaña central flotante
            [scene-con-tomate-center (place-image tomate-img tomate-center-x TOMATE3-CENTER-Y scene-con-tomate-left)]
            
            ; Estrella (solo si no ha sido recolectada)
            [scene-con-estrellita (if (not star-collected?)
                                      (place-image estrellita-img ESTRELLITA3-X ESTRELLITA3-Y scene-con-tomate-center)
                                      scene-con-tomate-center)]
            
            [player (player-image crouched?)]
            [scene-con-player (place-image player x y scene-con-estrellita)]
            
            ; Tiempo con fondo
            [time-bg (rectangle 120 30 "solid" (color 0 0 0 150))]
            [scene-con-tiempo (place-image (text (string-append "TIME: " (number->string (inexact->exact (floor level-time)))) 20 'white)
                                           70 30
                                           (place-image time-bg 70 30 scene-con-player))]
            
            ; Indicador de dificultad EXTREMA
            [diff-bg (rectangle 200 30 "solid" (color 139 0 0 180))]
            [scene-con-diff (place-image (text "EXTREME DIFFICULTY" 18 'white)
                                         650 30
                                         (place-image diff-bg 650 30 scene-con-tiempo))])
       
       (cond
         [level-complete?
          (overlay (overlay (above (text "★ NIVEL COMPLETADO ★" 48 "gold")
                                   (text "¡ERES UN SUPER ONION!" 28 "white")
                                   (text "Presiona ESC para salir" 20 "lightgray"))
                            (rectangle 650 180 "solid" (color 0 0 0 220)))
                   scene-con-diff)]
         
         [game-over?
          (overlay (draw-failed-level-message) scene-con-diff)]
         
         [else scene-con-diff]))]

    [else
     (empty-scene WIDTH HEIGHT)]))

; ============================================================
; TICK HANDLER ACTUALIZADO PARA NIVEL 3
; ============================================================

(define (tick-handler w)
  (cond
    [(member (hash-ref w 'mode) '(nivel1 nivel2 nivel3))
     (let* ([vy (hash-ref w 'vy)]
            [x  (hash-ref w 'x)]
            [y  (hash-ref w 'y)]
            [on-floor? (hash-ref w 'on-floor?)]
            [game-over? (hash-ref w 'game-over?)]
            [level-time (hash-ref w 'level-time)]
            [level-complete? (hash-ref w 'level-complete?)]
            [tomate-x (hash-ref w 'tomate-x)]
            [tomate-dir (hash-ref w 'tomate-dir)]
            [tomate-center-x (hash-ref w 'tomate-center-x)]
            [tomate-center-dir (hash-ref w 'tomate-center-dir)]
            [star-collected? (hash-ref w 'star-collected?)]
            [last-key (hash-ref w 'last-key)]
            [mode (hash-ref w 'mode)]
            
            [new-time (+ level-time TICK-SEC)]
            
            [new-vy (+ vy GRAVITY)]
            [new-y  (+ y new-vy)]
            
            [player-img (player-image (hash-ref w 'crouched?))]
            [player-height (image-height player-img)]
            [player-width (image-width player-img)]
            
            [horizontal-move (cond
                               [(string=? last-key "left") (- WALK-DX)]
                               [(string=? last-key "right") WALK-DX]
                               [else 0])]
            
            [proposed-x (+ x horizontal-move)]
            [proposed-y new-y]
            
            [on-main-floor? (>= (+ proposed-y (/ player-height 2)) FLOOR-Y)]
            
            ; LISTA DE PLATAFORMAS SEGÚN NIVEL - ACTUALIZADO PARA NIVEL 3
            [platforms (cond
                         [(eq? mode 'nivel1)
                          (list (list MONTANITA-X MONTANITA-Y MONTANITA-WIDTH MONTANITA-HEIGHT))]
                         
                         [(eq? mode 'nivel2)
                          (list (list MONTANITA1-X MONTANITA1-Y MONTANITA-WIDTH MONTANITA-HEIGHT)
                                (list MONTANITA2-X MONTANITA2-Y MONTANITA-WIDTH MONTANITA-HEIGHT))]
                         
                         [(eq? mode 'nivel3)
                          ; Tres montañas en forma de pirámide
                          (list (list MONTANITA3-LEFT-X MONTANITA3-LEFT-Y MONTANITA-WIDTH MONTANITA-HEIGHT)
                                (list MONTANITA3-RIGHT-X MONTANITA3-RIGHT-Y MONTANITA-WIDTH MONTANITA-HEIGHT)
                                (list MONTANITA3-CENTER-X MONTANITA3-CENTER-Y MONTANITA-WIDTH MONTANITA-HEIGHT))]
                         
                         [else '()])]
            
            ; Verificar colisiones verticales con plataformas
            [platform-vertical-collisions
             (filter (lambda (platform)
                       (let ([px (first platform)]
                             [py (second platform)]
                             [pw (third platform)]
                             [ph (fourth platform)])
                         (check-platform-collision proposed-x proposed-y player-width player-height px py pw ph)))
                     platforms)]
            
            ; Verificar si está sobre alguna plataforma
            [on-any-platform? (ormap (lambda (platform)
                                       (let ([px (first platform)]
                                             [py (second platform)]
                                             [pw (third platform)]
                                             [ph (fourth platform)])
                                         (check-on-top-of-platform? x y player-width player-height px py pw ph)))
                                     platforms)]
            
            ; Determinar posición Y final
            [final-y (cond
                       [on-main-floor? (- FLOOR-Y (/ player-height 2))]
                       [(not (empty? platform-vertical-collisions))
                        (let* ([platform (first platform-vertical-collisions)]
                               [py (second platform)]
                               [ph (fourth platform)])
                          (- py (/ ph 2) (/ player-height 2)))]
                       [else proposed-y])]
            
            [final-vy (cond
                        [(or on-main-floor? (not (empty? platform-vertical-collisions))) 0]
                        [else new-vy])]
            
            ; Verificar colisiones LATERALES con plataformas
            [lateral-collisions (filter (lambda (platform)
                                          (let ([px (first platform)]
                                                [py (second platform)]
                                                [pw (third platform)]
                                                [ph (fourth platform)])
                                            (check-side-collision proposed-x proposed-y player-width player-height px py pw ph)))
                                        platforms)]
            
            ; Determinar posición X final (sin colisiones laterales)
            [final-x (if (empty? lateral-collisions)
                         proposed-x
                         x)]
            
            ; Asegurar que no se salga de los límites de la pantalla
            [final-x (max 20 (min (- WIDTH 20) final-x))]
            
            ; Verificar colisiones con estrellita
            [star-positions (cond
                              [(eq? mode 'nivel1) (list ESTRELLITA-X ESTRELLITA-Y)]
                              [(eq? mode 'nivel2) (list ESTRELLITA2-X ESTRELLITA2-Y)]
                              [(eq? mode 'nivel3) (list ESTRELLITA3-X ESTRELLITA3-Y)])]
            
            [star-collision? (and (not star-collected?)
                                  (check-star-collision final-x final-y player-width player-height
                                                       (first star-positions) (second star-positions)
                                                       ESTRELLITA-SIZE))]
            
            [new-star-collected? (or star-collected? star-collision?)]
            [new-level-complete? (or level-complete? star-collision?)]
            
            [tomate-collision? #f]
            [new-tomate-x tomate-x]
            [new-tomate-dir tomate-dir]
            [new-tomate-center-x tomate-center-x]
            [new-tomate-center-dir tomate-center-dir])
       
       (let ([result-world
              (if (or game-over? new-level-complete?)
                  (hash-set* w
                             'vy final-vy
                             'y final-y
                             'x final-x
                             'on-floor? (or on-main-floor? (not (empty? platform-vertical-collisions)))
                             'game-over? game-over?
                             'level-time new-time
                             'level-complete? new-level-complete?
                             'tomate-x new-tomate-x
                             'tomate-dir new-tomate-dir
                             'tomate-center-x new-tomate-center-x
                             'tomate-center-dir new-tomate-center-dir
                             'star-collected? new-star-collected?
                             'last-key last-key)
                  (let ([current-game-over? game-over?]
                        [current-tomate-collision? #f]
                        [current-tomate-x tomate-x]
                        [current-tomate-dir tomate-dir]
                        [current-tomate-center-x tomate-center-x]
                        [current-tomate-center-dir tomate-center-dir])
                    
                    ; Nivel 2: tomate que se mueve
                    (when (eq? mode 'nivel2)
                      (set! current-tomate-x (+ current-tomate-x (* current-tomate-dir 1.5)))
                      
                      (when (or (>= current-tomate-x TOMATE-MAX-X)
                                (<= current-tomate-x TOMATE-MIN-X))
                        (set! current-tomate-dir (* current-tomate-dir -1))
                        (set! current-tomate-x (max TOMATE-MIN-X (min TOMATE-MAX-X current-tomate-x))))
                      
                      (set! current-tomate-collision? 
                            (check-enemy-collision final-x final-y player-width player-height
                                                   current-tomate-x TOMATE-Y TOMATE-SIZE TOMATE-SIZE)))
                    
                    ; Nivel 3: DOS tomates que se mueven (izquierda y centro)
                    (when (eq? mode 'nivel3)
                      ; Tomate en montaña izquierda
                      (set! current-tomate-x (+ current-tomate-x (* current-tomate-dir 1.8))) ; Un poco más rápido
                      
                      (when (or (>= current-tomate-x TOMATE3-LEFT-MAX-X)
                                (<= current-tomate-x TOMATE3-LEFT-MIN-X))
                        (set! current-tomate-dir (* current-tomate-dir -1))
                        (set! current-tomate-x (max TOMATE3-LEFT-MIN-X (min TOMATE3-LEFT-MAX-X current-tomate-x))))
                      
                      ; Tomate en montaña central
                      (set! current-tomate-center-x (+ current-tomate-center-x (* current-tomate-center-dir 1.6)))
                      
                      (when (or (>= current-tomate-center-x TOMATE3-CENTER-MAX-X)
                                (<= current-tomate-center-x TOMATE3-CENTER-MIN-X))
                        (set! current-tomate-center-dir (* current-tomate-center-dir -1))
                        (set! current-tomate-center-x (max TOMATE3-CENTER-MIN-X (min TOMATE3-CENTER-MAX-X current-tomate-center-x))))
                      
                      ; Verificar colisiones con AMBOS tomates
                      (set! current-tomate-collision? 
                            (or current-tomate-collision?
                                (check-enemy-collision final-x final-y player-width player-height
                                                       current-tomate-x TOMATE3-LEFT-Y TOMATE-SIZE TOMATE-SIZE)
                                (check-enemy-collision final-x final-y player-width player-height
                                                       current-tomate-center-x TOMATE3-CENTER-Y TOMATE-SIZE TOMATE-SIZE))))
                    
                    (hash-set* w
                               'vy final-vy
                               'y final-y
                               'x final-x
                               'on-floor? (or on-main-floor? (not (empty? platform-vertical-collisions)))
                               'game-over? (or current-game-over? current-tomate-collision?)
                               'level-time new-time
                               'level-complete? new-level-complete?
                               'tomate-x current-tomate-x
                               'tomate-dir current-tomate-dir
                               'tomate-center-x current-tomate-center-x
                               'tomate-center-dir current-tomate-center-dir
                               'star-collected? new-star-collected?
                               'last-key last-key)))])
         result-world))]
    
    [else w]))

; ============================================================
; KEY HANDLER (sin cambios)
; ============================================================

(define (key-handler w key)
  (cond
    [(eq? (hash-ref w 'mode) 'menu)
     (if (or (string=? key "enter") (string=? key " "))
         (hash-set w 'mode 'menu-niveles)
         w)]

    [(eq? (hash-ref w 'mode) 'menu-niveles)
     (cond
       [(string=? key "1")
        (make-world 'nivel1 CEBOLLITA-INICIAL-X (- FLOOR-Y (/ CEBOLLITA-HEIGHT 2)) 0 #t #f #f 0 #f 
                    TOMATE3-LEFT-X 1 TOMATE3-CENTER-X 1 #f "none")]
       [(string=? key "2")
        (make-world 'nivel2 CEBOLLITA-INICIAL-X (- FLOOR-Y (/ CEBOLLITA-HEIGHT 2)) 0 #t #f #f 0 #f 
                    TOMATE3-LEFT-X 1 TOMATE3-CENTER-X 1 #f "none")]
       [(string=? key "3")
        (make-world 'nivel3 CEBOLLITA-INICIAL-X (- FLOOR-Y (/ CEBOLLITA-HEIGHT 2)) 0 #t #f #f 0 #f 
                    TOMATE3-LEFT-X 1 TOMATE3-CENTER-X 1 #f "none")]
       [(string=? key " ")
        (hash-set w 'mode 'menu)]
       [else w])]

    [(eq? (hash-ref w 'mode) 'nivel1)
     (cond
       [(string=? key "left") (hash-set w 'last-key "left")]
       [(string=? key "right") (hash-set w 'last-key "right")]
       [(string=? key "up") (if (hash-ref w 'on-floor?)
                                (hash-set* w 'vy JUMP-VY 'on-floor? #f)
                                w)]
       [(string=? key "down") (hash-set w 'crouched? #t)]
       [(string=? key "escape") (make-world 'menu-niveles CEBOLLITA-INICIAL-X FLOOR-Y 0 #t #f #f 0 #f 
                                           TOMATE3-LEFT-X 1 TOMATE3-CENTER-X 1 #f "none")]
       [(string=? key "r") (make-world 'nivel1 CEBOLLITA-INICIAL-X (- FLOOR-Y (/ CEBOLLITA-HEIGHT 2)) 0 #t #f #f 0 #f 
                                      TOMATE3-LEFT-X 1 TOMATE3-CENTER-X 1 #f "none")]
       [else w])]
    
    [(member (hash-ref w 'mode) '(nivel2 nivel3))
     (cond
       [(string=? key "left") (hash-set w 'last-key "left")]
       [(string=? key "right") (hash-set w 'last-key "right")]
       [(string=? key "up") (if (hash-ref w 'on-floor?)
                                (hash-set* w 'vy JUMP-VY 'on-floor? #f)
                                w)]
       [(string=? key "down") (hash-set w 'crouched? #t)]
       [(string=? key "escape") (make-world 'menu-niveles CEBOLLITA-INICIAL-X FLOOR-Y 0 #t #f #f 0 #f 
                                           TOMATE3-LEFT-X 1 TOMATE3-CENTER-X 1 #f "none")]
       [(string=? key "r") (make-world (hash-ref w 'mode) CEBOLLITA-INICIAL-X (- FLOOR-Y (/ CEBOLLITA-HEIGHT 2)) 0 #t #f #f 0 #f 
                                      TOMATE3-LEFT-X 1 TOMATE3-CENTER-X 1 #f "none")]
       [else w])]
    
    [else w]))

(define (release-handler w key)
  (cond
    [(member (hash-ref w 'mode) '(nivel1 nivel2 nivel3))
     (cond
       [(or (string=? key "left") (string=? key "right"))
        (if (string=? key (hash-ref w 'last-key))
            (hash-set w 'last-key "none")
            w)]
       [(string=? key "down") (hash-set w 'crouched? #f)]
       [else w])]
    
    [else w]))

(define (mouse-handler w x y event)
  (cond
    [(and (eq? (hash-ref w 'mode) 'menu)
          (eq? event "button-down"))
     (hash-set w 'mode 'menu-niveles)]

    [(and (eq? (hash-ref w 'mode) 'menu-niveles)
          (eq? event "button-down"))
     (cond
       [(inside? x y 200 300 120 80)
        (make-world 'nivel1 CEBOLLITA-INICIAL-X (- FLOOR-Y (/ CEBOLLITA-HEIGHT 2)) 0 #t #f #f 0 #f 
                    TOMATE3-LEFT-X 1 TOMATE3-CENTER-X 1 #f "none")]
       [(inside? x y 400 300 120 80)
        (make-world 'nivel2 CEBOLLITA-INICIAL-X (- FLOOR-Y (/ CEBOLLITA-HEIGHT 2)) 0 #t #f #f 0 #f 
                    TOMATE3-LEFT-X 1 TOMATE3-CENTER-X 1 #f "none")]
       [(inside? x y 600 300 120 80)
        (make-world 'nivel3 CEBOLLITA-INICIAL-X (- FLOOR-Y (/ CEBOLLITA-HEIGHT 2)) 0 #t #f #f 0 #f 
                    TOMATE3-LEFT-X 1 TOMATE3-CENTER-X 1 #f "none")]
       [else w])]

    [else w]))

(define (should-stop w) #f)

(big-bang initial-world
          (on-tick tick-handler TICK-SEC)
          (on-key key-handler)
          (on-release release-handler)
          (on-mouse mouse-handler)
          (to-draw draw-world)
          (stop-when should-stop))