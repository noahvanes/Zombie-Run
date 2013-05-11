(let ((port (open-output-file "board")))
  (write 
   '(begin  
      (define PIOA  #xFFFFF40)
      (define PIOB  #xFFFFF60)
      (define PIO_PER #x0000)
      (define PIO_OER #x0010)
      (define PIO_ODR #x0014)
      (define PIO_PDR #x0004)
      (define PIO_ASR #x0070)
      (define LCD_BL 20)
      (define J_LEFT 7)
      (define J_UP 9)
      (define J_RIGHT 14)
      (define J_DOWN 8)
      (define ash bitwise-arithmetic-shift)
      (define (enable-io-pin! port pin)
        (write (ash 1 pin) port PIO_PER))
      (define (disable-io-pin! port pin)
        (write (ash 1 pin) port PIO_PDR))
      (define (set-as-output-pin! port pin)
        (write (ash 1 pin) port PIO_OER))
      (define (set-as-input-pin! port pin)
        (write (ash 1 pin) port PIO_ODR))
      (define (set-pin-purpose-a! port pin)
        (write (ash 1 pin) port PIO_ASR))
      ) port)
  (close-output-port port))

(let ((port (open-output-file "lcd-data")))
  (write 
   '(begin  
      (define SPI_0 #xFFFE000)
      (define SCK0 18) 
      (define MOSI0 17)
      (define MISO0 16)
      (define CS_LCD 12) 
      (define LCD_RESET 2)
      (define LCD_BL 20)
      (define SPIO0_NPCS1 13)
      (define SPI_CSR0 #x30)
      (define SPI_CSR1 #x34)
      (define ID_SPI0 4)
      (define PMC #xFFFFFC0)
      (define PMC_PCER #x0010)
      (define SPI_CR #x0)
      (define SPI_MR #x04)
      (define DISCTL    #xCA)
      (define COMSCN    #xBB)
      (define OSCON     #xD1)
      (define SLPOUT    #x94)
      (define VOLCTR    #x81)
      (define TMPGRD    #x82)
      (define PWRCTR    #x20)
      (define DISNOR    #xA6)
      (define DISINV    #xA7)
      (define PTLOUT    #xA9)
      (define DATCTL    #xBC)
      (define NOP       #x25)
      (define PASET     #x75)
      (define CASET     #x15)
      (define RAMWR     #x5C)
      (define DISON   #xAF)) port)
  (close-output-port port))

 

(let ((port (open-output-file "lcd")))
  (write 
   '(begin   
      (define (LCD_RESET_LOW)  (clear-pin! PIOA LCD_RESET))
      (define (LCD_RESET_HIGH) (set-pin! PIOA LCD_RESET))
      (define (wait x) x)
      (define (InitLCD)
        (enable-io-pin! PIOB LCD_BL)
        (set-as-output-pin! PIOB LCD_BL)
        (set-pin! PIOB LCD_BL)
        (enable-io-pin! PIOA LCD_RESET)
        (set-as-output-pin! PIOA LCD_RESET)
        (set-pin! PIOA LCD_RESET)
        (for-each (lambda (p) (disable-io-pin! PIOA p))
                  (list CS_LCD MISO0 MOSI0 SCK0 SPIO0_NPCS1))
        (for-each (lambda (p) (set-pin-purpose-a! PIOA p))
                  (list CS_LCD MISO0 MOSI0 SCK0 SPIO0_NPCS1))
        (write (ash 1 ID_SPI0) PMC PMC_PCER)
        (write #x80 SPI_0 SPI_CR)
        (write #x01 SPI_0 SPI_CR)
        (write #x100E0011 SPI_0 SPI_MR)
        (write #x01010C11 SPI_0 SPI_CSR0)
        (write #x01010502 SPI_0 SPI_CSR1))
      (define (WriteSpiData data) 
        (spi-put SPI_0  (bitwise-ior data #x0100)))
      (define (WriteSpiCommand! cmd . args)
        (spi-put SPI_0 (bitwise-and cmd #xFFFFFEFF))
        (for-each WriteSpiData args))
      (define (LCDSettings) 
        (LCD_RESET_LOW)
        (wait 1000)
        (LCD_RESET_HIGH)
        (wait 1000)
        (WriteSpiCommand! DISCTL #x00 #x20 #x0a)
        (WriteSpiCommand! COMSCN #x00)
        (WriteSpiCommand! OSCON)
        (WriteSpiCommand! SLPOUT)
        (WriteSpiCommand! VOLCTR 43 3)
        (WriteSpiCommand! TMPGRD  #x00)
        (WriteSpiCommand! PWRCTR #x0f)
        (WriteSpiCommand! DISNOR)
        (WriteSpiCommand! DISINV)
        (WriteSpiCommand! PTLOUT)
        (WriteSpiCommand! DATCTL #x00 #x03 #x02)
        (WriteSpiCommand! NOP)
        (WriteSpiCommand! DISON))) port)
  (close-output-port port))

(let ((port (open-output-file "lcd-f")))
  (write 
   '(begin  
      (define (fill-rectangle! x y width height colour)
        (let* ((x1 x)
               (x2 (+ x width -1))
               (y1 (+ y 2))
               (y2 (+ y height 1))
               (i (round (+ 130 (/ (* (+ 1 (- (+ x width) x1))  (+ 1 (- (+ y height) y))) 2)))))
          (WriteSpiCommand! PASET y1 y2)
          (WriteSpiCommand! CASET x1 x2)
          (WriteSpiCommand! RAMWR)         
          (spi-put-n SPI_0 colour i))))
      port)
  (close-output-port port))


(let ((port (open-output-file "start-up")))
  (write 
   '(begin  
      (load "board")
      (load "lcd-data")
      (load "lcd")
      (load "lcd-f")
      (InitLCD)
      (LCDSettings))
   port)
  (close-output-port port))