#lang racket

;; ==========================
;; INICIO Y LIBRERÍAS
;; ==========================

; Librería de figuras, fondos, botones, ventanas...
(require graphics/graphics)

; Librería de sonido principalmente
; GUI: Graphical User Interface
(require racket/gui)

; Librería de rutas de archivos sin errores (cargar multimedia directo del explorador de archivos)
; PATH: 
(require racket/runtime-path)



;; ==========================
;; RUTAS DE ARCHIVOS
;; ==========================


; Función para que los archivos funcionen desde cualquier dispositivo
(define-runtime-path sonido-path
  "sonidos proyecto/cinematica/cinematica.wav")

; PLAY-SOUND: Función para el sonido, de (racket/gui)
; PATH: "Dirección" del archivo
; #t: Reproducir sin bloquear el programa (sonar mientras pasan otras cosas)
(define (sonido-cinematica)
  (play-sound sonido-path #t))



;; ==========================
;; VENTANA PRINCIPAL
;; ==========================

; Iniciar programa
(open-graphics)

; Medidas de la ventana
; WIDTH: Ancho
; HEIGTH: Alto
(define WIDTH  900)
(define HEIGHT 800)

;; Abrir la ventana
(define vent (open-viewport "FARAFALLA" WIDTH HEIGHT))



;; ==========================
;; PANTALLA PORTADA 1 (solo play)
;; ==========================
(define BTN-X 500)   ; posición X del botón
(define BTN-Y 600)   ; posición Y del botón
(define BTN-W 280)   ; ancho del botón
(define BTN-H 112)   ; alto del botón

;; Dibuja fondo azul (general)
(define (dibujar-fondo!)
  ((draw-solid-rectangle vent)
   (make-posn 0 0)
   WIDTH HEIGHT
   (make-rgb 0.25 0.71 1.0)))

;; Dibuja la imagen del botón (general)
(define (dibujar-boton-play!)
  ((draw-pixmap vent)
   "imagenes proyecto/Boton play y loading/play boton.bmp"
   (make-posn BTN-X BTN-Y)
   "black"))

;; (si quieres usar la detección exacta del botón más tarde,
;; puedes dejarla; si no, la puedes borrar)
(define (click-en-boton? pos)
  (and (posn? pos)
       (let ([x (posn-x pos)]
             [y (posn-y pos)])
         (and (<= BTN-X x (+ BTN-X BTN-W))
              (<= BTN-Y y (+ BTN-Y BTN-H))))))

;; Espera hasta que hagan click
(define (esperar-click-valido)
  (get-mouse-click vent))


;; ==========================
;; PANTALLA INICIO (loading + play)
;; ==========================
(define (pantalla-inicio)
  (clear-viewport vent)

  (dibujar-fondo!)        ; ✅ usamos la función de fondo
  (dibujar-boton-play!)   ; ✅ usamos la función de botón

  ;; Esperar UN click y pasar a la portada
  (esperar-click-valido)
  (pantalla-portada))



;; ==========================
;; PANTALLA PORTADA 2 (corazón + logo)
;; ==========================
(define (pantalla-portada)
  (clear-viewport vent)
  ;; Fondo azul
  ((draw-solid-rectangle vent)
   (make-posn 0 0)
   WIDTH HEIGHT
   (make-rgb 0.25 0.7 1.0))
  ;; Corazón con buhitos
  ((draw-pixmap vent)
   "imagenes proyecto/Buhos juntos/buhitos inicio.bmp"
   (make-posn 40 40)
   "black")
  ;; Logo FARAFALLA
  ((draw-pixmap vent)
   "imagenes proyecto/Logo/farafalla logo.bmp"
   (make-posn 418 370)
   "black")
  ;; Botón play de esta pantalla
  ((draw-pixmap vent)
   "imagenes proyecto/Boton play solo/boton pantalla 2.bmp"
   (make-posn 520 600)
   "black")
  ;; Tu nombre 💜
  ((draw-pixmap vent)
   "imagenes proyecto/Mi nombre/empleos.bmp"
   (make-posn 470 470)
   "black")

  (esperar-click-valido)
  (pantalla-cinematica-1)

  'ok)

;; ==========================
;; PANTALLA CINEMÁTICA 1
;; ==========================
(define (pantalla-cinematica-1)
  (clear-viewport vent)

  ;; Fondo negro
  ((draw-solid-rectangle vent)
   (make-posn 0 0)
   WIDTH HEIGHT
   (make-rgb 0 0 0))

  ;; Texto centrado (imagen BMP)
  ((draw-pixmap vent)
   "imagenes proyecto/Cinematica 1/texto 1.bmp"
   (make-posn 240 350)
   "black")

  ;; Música de la cinemática
  (sonido-cinematica)

  ;; Pasar a la siguiente pantalla
  (sleep 2.5)
  (pantalla-cinematica-2))


;; ==========================
;; PANTALLA CINEMÁTICA 2 (buhito cayendo al derecho)
;; ==========================
(define (pantalla-cinematica-2)
  (clear-viewport vent)

  ;; POSICIONES DEL BICHITO (empieza arriba)
  (define x 400)    ; posición horizontal del muñeco
  (define y -150)   ; empieza más arriba para que no aparezca cortado

  ;; Dibujar el bichito quietico arriba
  ((draw-pixmap vent)
   "imagenes proyecto/Buhito/bicho de frente.bmp"
   (make-posn 400 -100)
   "black")
  
  ;; Bucle para hacerlo caer
  (let loop ()
    (when (< y (+ HEIGHT 100))   ; cae hasta salir por abajo
      (clear-viewport vent)

      ;; Fondo azul
      ((draw-solid-rectangle vent)
       (make-posn 0 0)
       WIDTH HEIGHT
       (make-rgb 0.25 0.7 1.0))

      ;; Dibujar el bichito cayendo
      ((draw-pixmap vent)
       "imagenes proyecto/Buhito/bicho de frente.bmp"
       (make-posn x y)
       "black")

      ;; Actualizar posición
      (set! y (+ y 10))

      (sleep 0.01)        ; suavidad de la animación
      (loop)))            ; repetir

    (sleep 1)
  (pantalla-cinematica-3)

  ;; Cuando termine la caída, pasa a la siguiente pantalla
  )

;; ==========================
;; PANTALLA CINEMÁTICA 3  (... I'm falling, falling ...)
;; ==========================
(define (pantalla-cinematica-3)
  (clear-viewport vent)

  ;; Fondo negro
  ((draw-solid-rectangle vent)
   (make-posn 0 0)
   WIDTH HEIGHT
   (make-rgb 0 0 0))

  ;; Texto (imagen BMP con "... I'm falling, falling ...")
  ((draw-pixmap vent)
   "imagenes proyecto/Cinematica 2/texto 2.bmp"
   (make-posn 240 370)
   "black")

  ;; Duración de esta pantalla (por ejemplo 3 segundos)
  (sleep 2.5)

  ;; 👇 aquí luego llamas a la siguiente parte del juego
(pantalla-cinematica-4)
  )

;; ==========================
;; PANTALLA CINEMÁTICA 4 (buhito cayendo al revés)
;; ==========================
(define (pantalla-cinematica-4)
  (clear-viewport vent)

  ;; POSICIONES DEL BICHITO (empieza arriba)
  (define x 400)    ; posición horizontal del muñeco
  (define y -150)   ; empieza más arriba para que no aparezca cortado

  ;; Dibujar el bichito quietico arriba
  ((draw-pixmap vent)
   "imagenes proyecto/Buhito/bicho de cabeza.bmp"
   (make-posn 400 -100)
   "black")
  
  ;; Bucle para hacerlo caer
  (let loop ()
    (when (< y (+ HEIGHT 100))   ; cae hasta salir por abajo
      (clear-viewport vent)

      ;; Fondo azul
      ((draw-solid-rectangle vent)
       (make-posn 0 0)
       WIDTH HEIGHT
       (make-rgb 0.25 0.7 1.0))

      ;; Dibujar el bichito cayendo
      ((draw-pixmap vent)
       "imagenes proyecto/Buhito/bicho de cabeza.bmp"
       (make-posn x y)
       "black")

      ;; Actualizar posición
      (set! y (+ y 10))

      (sleep 0.01)        ; suavidad de la animación
      (loop)))            ; repetir

    (sleep 1)
  (pantalla-cinematica-5)

  ;; Cuando termine la caída, pasa a la siguiente pantalla
  )

;; ==========================
;; PANTALLA CINEMÁTICA 5 (final)
;; ==========================

(define (pantalla-cinematica-5)
  (clear-viewport vent)

  ;; Fondo negro
  ((draw-solid-rectangle vent)
   (make-posn 0 0)
   WIDTH HEIGHT
   (make-rgb 0 0 0))

  ;; Texto centrado (imagen BMP)
  ((draw-pixmap vent)
   "imagenes proyecto/Logo/farafalla logo negro.bmp"
   (make-posn 200 250)
   "black")

  ((draw-pixmap vent)
   "imagenes proyecto/Mi nombre/nombre cinematica final.bmp"
   (make-posn 240 440)
   "black")

  (sleep 3.5))

(pantalla-inicio)

;; ==========================
;; CONSTANTES GENERALES
;; ==========================

(define ANCHO 900)
(define ALTO 800)
(define LIM-IZQ (* 0.3 ANCHO))  ; límite izquierdo (30% de la pantalla)
(define LIM-DER (* 0.7 ANCHO))  ; límite derecho (70% de la pantalla)

;; Tamaño del bichito (para dibujar)
(define BICHO-ANCHO 20)
(define BICHO-ALTO 78)   ;; alto aprox del cuerpo
;; El piso está justo en la mitad de la pantalla
(define PISO-Y 350)

;; Velocidades y física
(define PASO-X 10)          ; caminar
(define GRAVEDAD 2)         ; caer
(define IMPULSO-SALTO -30)  ; salto
(define DT 0.02)            ; tiempo entre frames


;; =======================
;; ESTADO DE CONTROLES
;; =======================

;; Tecla horizontal actual: 'left, 'right o 'none
(define current-h-key 'none)

;; ¿Hay un salto pendiente? (se marca al oprimir ↑)
(define jump-request? #f)

;; Hilo que escucha las teclas sin bloquear el juego
(define (teclado-loop)
  (let* ([evt (get-key-press vent)]
         [k   (key-value evt)])
    (cond
      [(eq? k 'left)   (set! current-h-key 'left)]
      [(eq? k 'right)  (set! current-h-key 'right)]
      [(eq? k 'up)     (set! jump-request? #t)]
      [else            (set! current-h-key 'none)])
    (teclado-loop)))

(thread teclado-loop)


;; =======================
;; DIBUJAR ESCENA 
;; =======================


(define (dibujar-escena x y)
  (clear-viewport vent)
  

; Cielo
  ((draw-solid-rectangle vent)
   (make-posn 0 0)
   ANCHO ALTO
   (make-rgb 0.25 0.7 1.0))
  

; Piso
  ((draw-solid-rectangle vent)
   (make-posn 0 PISO-Y)
   ANCHO (- ALTO PISO-Y)
   (make-rgb 0.15 0.25 0.15))
  
  
  (let* ([x-izq (- x (/ BICHO-ANCHO 2))]
         [y-arriba (- y BICHO-ALTO)])
    ((draw-pixmap vent)
     "imagenes proyecto/Buhito/bicho de frente.bmp"
     (make-posn x-izq y-arriba)
     "black")))


;; =======================
;; CAÍDA INICIAL
;; =======================

(define (caida-inicial)
  (define x (/ ANCHO 2))   ; centro
  (define y -200)          ; empieza bien arriba
  (define vy 0)

  (let loop ()
    (dibujar-escena x y)
    (sleep DT)

    (set! vy (+ vy GRAVEDAD))
    (set! y (+ y vy))

    (if (< y PISO-Y)
        (loop)
        (values x PISO-Y))) ) ; posición final sobre el piso


;; =======================
;; LOOP DEL JUEGO
;; =======================

(define (loop x y vy en-el-aire?)
  (dibujar-escena x y)
  (sleep DT)

  ;; ----- movimiento horizontal según la tecla guardada -----
  (define dx
    (cond [(eq? current-h-key 'left)  (- PASO-X)]
          [(eq? current-h-key 'right) PASO-X]
          [else 0]))

  ;; ----- salto: solo una vez por pulsación -----
  (define-values (vy1 en-el-aire1)
    (cond
      [(and jump-request? (not en-el-aire?))
       (set! jump-request? #f)           ; consume el salto
       (values IMPULSO-SALTO #t)]
      [else
       (values vy en-el-aire?)]))
  

  ;; ----- gravedad -----
  (define vy2 (+ vy1 GRAVEDAD))
  

  ;; ----- actualizar Y con colisión piso -----
  (define y-temp (+ y vy2))
  (define y1 (if (> y-temp PISO-Y) PISO-Y y-temp))

  (define vy-final   (if (= y1 PISO-Y) 0 vy2))
  (define en-el-aire-final (not (= y1 PISO-Y)))
  

  ;; ----- actualizar X con bordes -----
  (define x1
  (min
   (max (+ x dx) LIM-IZQ)
   LIM-DER))

  (loop x1 y1 vy-final en-el-aire-final))



;; =======================
;; INICIO DEL JUEGO
;; =======================

(let-values ([(x0 y0) (caida-inicial)])
  (loop x0 y0 0 #f))

