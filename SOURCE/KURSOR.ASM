DOSSEG
.MODEL TPASCAL

.CODE
PROC KUROFF FAR
      PUBLIC KUROFF
      push bp
      mov bp, sp
      mov ah, 2
      mov dl, 0
      mov bh, 0
      mov dh, 25
      int 10h
      pop bp
      ret
      ENDP

PROC KURON FAR
      PUBLIC KURON
      push bp
      mov bp, sp
      mov ah, 2
      mov dx, 0
      mov bh, 0
      int 10h
      pop bp
      ret
      ENDP
      END

