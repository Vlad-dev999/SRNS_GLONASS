uses dos, crt;

const Ti=10;
type OnOff=( On, Off );

var Tc, Bc: byte;

procedure Win ( x1, y1, x2, y2, Bc, Tc : byte ) ;
begin
   Window( x1, y1, x2, y2 );
   TextBackground( Bc );TextColor( Tc )
end;

procedure Mulk_Yes( sX, sY: byte;   {   Size  from  X, Y   }
                    cX, cY: byte;   {   Central Point      }
              BackC, TextC: byte );
var aX, aY, i: byte;
begin
   for i:=1 to Ti do
      begin
         aX:=Round( i*sX/Ti ) div 2;     aY:=Round( i*sY/Ti ) div 2;
         win( cX+2-aX, cY+1-aY, cX+2+aX, cY+1+aY, Black, Black );
         clrscr;
         win( cX-aX, cY-aY, cX+aX, cY+aY, BackC, TextC );
         clrscr;   delay( 30 )
      end
end;

procedure Mulk_No( sX, sY: byte;   {   Size  from  X, Y   }
                   cX, cY: byte;   {   Central Point      }
             BackC, TextC: byte );
var aX, aY, i: byte;
begin
   for i:=Ti downto 1 do
      begin
         aX:=Round( i*sX/Ti ) div 2;     aY:=Round( i*sY/Ti ) div 2;
         win( cX+2-aX, cY+1-aY, cX+2+aX, cY+1+aY, Black, Black );
         clrscr;
         win( cX-aX, cY-aY, cX+aX, cY+aY, BackC, TextC );
         clrscr;   delay( 15 );
         win( cX-aX, cY-aY, cX+2+aX, cY+2+aY, Bc, Tc );   clrscr;
      end;
end;

procedure Kur( Signum: OnOff );
var Regs: Registers;
begin
   with Regs do
      begin
         AH:=2;     DL:=0;   BH:=0;
         Case Signum of
               Off:  DH:=25;
                On:  DH:=0
            end
      end;
   Intr( $10, Regs )
end;

procedure Title;
const cod :array [1..11] of byte=( 201, 187, 188, 200, 205, 186, 203,
                                  185, 202, 204, 206 );
      name:array [1..15] of byte=( 75, 97,  115, 104,  99, 104, 101,
                                  110, 107,  111,  32,  86,  46,  83, 46 );
      fg=18;     lg=62;
        yg:array [1..2] of byte=( 8, 15 );
      fv=9 ;     lv=14;
        xv:array [1..2] of byte=( 17, 63 );
        x1:array [1..4] of byte=( 17, 63, 63, 17 );
        y1:array [1..4] of byte=( 8, 8, 15, 15 );
var l, x, y: byte ;
    c    : char ;
    s    : string[15] ;
begin
   GotoXY( 34, 23 );           Write( 'Press any key' );
   s:='';                      for x:=1 to 15 do s:=s+Chr( name[x] );
   Mulk_Yes( 46, 7, 40, 12, Cyan, LightMagenta );
   ClrScr;                     Window( 1, 1, 80, 25 );
   for l:=1 to 2 do for x:=fg to lg do
      begin
         GotoXY( x, yg[l] );   Write( Chr( cod[5] ) )
      end;
   for l:=1 to 2 do for y:=fv to lv do
      begin
         GotoXY( xv[l], y );   Write( Chr( cod[6] ) )
      end;
   for l:=1 to 4 do
      begin
         GotoXY( x1[l], y1[l] );    Write( Chr( cod[l] ) )
      end;
   Win( 18, 9, 62, 14, 7, 10 );
   ClrScr;           Writeln;
   Write( '  Attestation Near-Earth Satellite by GLONASS' );
   TextColor( 5 );
   GotoXY( 3, 2 );     Write( 'A' );      GotoXY( 15, 2 );     Write( 'N' );
   GotoXY( 20, 2 );    Write( 'E' );      GotoXY( 25, 2 );     Write( 'S' );
   GotoXY( 38, 2 );    Write( 'G' );
   Writeln;            TextColor( 1 );
   Writeln( '                 Version 2.0 ' );
   Writeln( '      (C) Copyright 1995, '+s+' ' );
   Write( '        Kharkov Aviation Institute/502 ' );
   Writeln;
   Kur( Off );
   c:=ReadKey;
   Mulk_No( 46, 8, 40, 11, Cyan, Red );
end;


begin
   Tc:=Black;      Bc:=Green;
   win(1, 1, 80, 25, Bc, Tc);     clrscr;
   Title
end.

