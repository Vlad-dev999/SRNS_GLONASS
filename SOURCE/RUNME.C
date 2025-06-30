#define _Cdecl	cdecl

#define P_WAIT	  0	/* child runs separately, parent waits until exit */
#define P_NOWAIT  1	/* both concurrent -- not implemented */
#define P_OVERLAY 2	/* child replaces parent, parent no longer exists */

#define NULL    0

#if !defined(FALSE) || !defined(TRUE)
#define FALSE   0
#define TRUE    1
#endif

typedef long clock_t;
#define CLK_TCK 18.2

#define V_ADRESS 0xB800
enum StateType { ACTIVE, PASSIVE, SELECT };
enum COLORS {
	BLACK,			/* dark colors */
	BLUE,
	GREEN,
	CYAN,
	RED,
	MAGENTA,
	BROWN,
	LIGHTGRAY,
	DARKGRAY,		/* light colors */
	LIGHTBLUE,
	LIGHTGREEN,
	LIGHTCYAN,
	LIGHTRED,
	LIGHTMAGENTA,
	YELLOW,
	WHITE
};

int	 _Cdecl peek	(unsigned segment, unsigned offset);
void	 _Cdecl poke	(unsigned segment, unsigned offset, int value);
void 	 _Cdecl clrscr	(void);
int	 _Cdecl putch	(int c);
void 	 _Cdecl textbackground	(int newcolor);
void 	 _Cdecl textcolor	(int newcolor);
void 	 _Cdecl window		(int left, int top, int right, int bottom);
int	 _Cdecl cputs		(const char *str);
char	*_Cdecl searchpath	(const char *file);
int	 _Cdecl getch		(void);
int	 _Cdecl spawnle         (int mode, char *path, char *arg0, ...);
int      _Cdecl printf          (const char *format, ...);
clock_t  _Cdecl clock(void);
int	 _Cdecl setcbrk	        (int cbrkvalue);
char   * _Cdecl getenv          (const char *name);
void 	 _Cdecl clreol		(void);


void win(int x1, int y1, int x2, int y2, int Tc, int Bc){
   textcolor(Tc);             textbackground(Bc);
   window(x1, y1, x2, y2);    clrscr();
}

void winb(int x1, int y1, int x2, int y2, unsigned Tc, unsigned Bc,
					  int TEXT,    int BACK){
   int i, y_1;
   unsigned Attrib, Offst;
   Attrib=((Bc << 4) + Tc) << 8;    y_1=y1;
   Offst=160*(y1-1)+(x1-1)*2;
   poke(V_ADRESS, Offst, Attrib+201);   Offst+=2;
   for (i=1; i < (x2-x1); i++, Offst+=2)
      poke(V_ADRESS, Offst, Attrib+205);
   poke(V_ADRESS, Offst, Attrib+187);
   y1++;
   do {
      Offst=160*(y1-1)+(x1-1)*2;
      poke(V_ADRESS, Offst, Attrib+186);   Offst+=2;
      for (i=1; i < (x2-x1); i++, Offst+=2)
	 poke(V_ADRESS, Offst, Attrib+32);
      poke(V_ADRESS, Offst, Attrib+186);
   }
   while (y1++ < y2);
   Offst=160*(y2-1)+(x1-1)*2;
   poke(V_ADRESS, Offst, Attrib+200);   Offst+=2;
   for (i=1; i < (x2-x1); i++, Offst+=2)
      poke(V_ADRESS, Offst, Attrib+205);
   poke(V_ADRESS, Offst, Attrib+188);
   win(x1+1, y_1+1, x2-1, y2-1, TEXT, BACK);
}

void Info(char *Mes){
   win(1, 25, 80, 25, RED, WHITE);
   cputs(Mes);
}

void menu(int Nmelm, int Sel, int x, int y, int Width, char* Mes,
	  unsigned TEXT, unsigned BACK){
   int Tc, Bc;
   switch (Sel) {
      case ACTIVE:    Tc=BACK;     Bc=TEXT;   break;
      case PASSIVE:   Tc=TEXT;     Bc=BACK;   break;
      case SELECT:    Tc=LIGHTRED; Bc=GREEN;  break;
   }
   win(x, y+Nmelm, x+Width-2, y+Nmelm, Tc, Bc);
   cputs(Mes);
}

void choise(int* NChoise, char** Armes,
	   int x1, int y1, int x2, int y2,
	   unsigned Tc, unsigned Bc, unsigned TEXT, unsigned BACK){
   int i=1, Exit=1;
   char ch;
   winb(x1, y1, x2, y2, Tc, Bc, TEXT, BACK);
   cputs( Armes[0] );   cputs( "" );
   menu(1, PASSIVE, x1+2, y1, x2-x1-2, Armes[0], TEXT, BACK);
   menu(2, PASSIVE, x1+2, y1, x2-x1-2, "", TEXT, BACK);
   menu(3, ACTIVE, x1+2, y1, x2-x1-2, Armes[1], TEXT, BACK);
   for (i=2; i <= *NChoise; i++)
      menu(i+2, PASSIVE, x1+2, y1, x2-x1-2, Armes[i], TEXT, BACK);
   if (*NChoise==1) Exit=0;
   i=1;
   do {
      if ((ch=getch())=='\x0') {
	 ch=getch();
         menu(i+2, PASSIVE, x1+2, y1, x2-x1-2, Armes[i], TEXT, BACK);
	 switch ((int)ch) {
	    case 80:   (*NChoise==i) ? i=1: i++;  break;
	    case 72:   (i==1) ? i=*NChoise: i--;  break;
	    case 81:   i=*NChoise;                break;
	    case 73:   i=1;                       break;
	 }
         menu(i+2, ACTIVE, x1+2, y1, x2-x1-2, Armes[i], TEXT, BACK);
      }  else  if (ch==13)
	       Exit=0,   menu(i+2, SELECT, x1+2, y1, x2-x1-2, Armes[i], TEXT, BACK);
   }
   while (Exit);
   *NChoise=i;
}

int main()
{
extern	int	 _Cdecl	 errno;
extern char    **_Cdecl  environ;
char                     *path, arg2[10]="";
clock_t                  Run_Time;
int i, j, k, EXIT, choose;
int x1=6, y1=7, x2=75, y2=17;
void* X;

const char* flag = "ллллллллл";

char* Message[] = {
      "                             Main Menu ",
      " Trass Near-Earth Satellite by Kepler Parameter`s..( Tr_NES.exe )",
      " Trass GLONASS Satellite`s by Almanah..............( Tr_NSS.exe )",
      " Attestation Near-Earth Satellite by GLONASS.......( NESAtt.exe )",
      " View Result Attestation Near-Earth Satellite......( Viewer.exe )",
      " Run all Program ",
      " Return to DOS.....................................(    Exit    )" };

char* arg0[] = { "TITLE.OVL",
                 "TR_NES.OVL",
                 "TR_NSS.OVL",
                 "NES_ATT.OVL",
                 "VIEWER.OVL"  };

i=setcbrk(1);
environ[2]=getenv("BREAK");
path=searchpath( arg0[0] );
k=spawnle( P_WAIT, path, "", NULL, environ );
EXIT=TRUE;
while( EXIT ){
   win( 1, 1, 80, 25, BLACK, BLUE );
   clrscr();                 choose=6;
   Info("   Choose working program ( keys: Up, Dn - move; ENTER - choose )  ");
   choise(&choose, Message, x1, y1, x2, y2, RED, GREEN, CYAN, BLACK);
   switch ( choose ) {
      case 1:  path=searchpath( arg0[1] );
               k=spawnle( P_WAIT, path, "", NULL, environ );    break;
      case 2:  path=searchpath( arg0[2] );
               k=spawnle( P_WAIT, path, "", NULL, environ );
         switch ( k ) {
            case 99:  path=searchpath( arg0[1] );
                      k=spawnle( P_WAIT, path, "", NULL, environ );
                      break;
            case 100: path=searchpath( arg0[1] );
                      k=spawnle( P_WAIT, path, "", "-w", NULL, environ );
                      break;
            default:  itoa( k-100, arg2, 10 );
                      path=searchpath( arg0[1] );
                      k=spawnle( P_WAIT, path, "", "-w", arg2, NULL, environ );
                      break;
         }    break;
      case 3:  path=searchpath( arg0[3] );
               k=spawnle( P_WAIT, path, "", NULL, environ );    break;
      case 4:  path=searchpath( arg0[4] );
               k=spawnle( P_WAIT, path, "", NULL, environ );    break;
      case 5:  for( j=1; j<=4; j++){
                   path=searchpath(arg0[j]);
                   k=spawnle( P_WAIT, path, "", NULL, environ );
                   switch ( k ) {
                     case 99:  path=searchpath( arg0[1] );
                               k=spawnle( P_WAIT, path, "", NULL, environ );
                               break;
                     case 100: path=searchpath( arg0[1] );
                               k=spawnle( P_WAIT, path, "", "-w", NULL, environ );
                               break;
                     default:  if (k < 100) break;
                               itoa( k-100, arg2, 10 );
                               path=searchpath( arg0[1] );
                               k=spawnle( P_WAIT, path, "", "-w", arg2, NULL, environ );
                               break;
                   }
               };   break;
      default:   EXIT=FALSE;
   } ;
}
   Run_Time=(int)(clock()/CLK_TCK);
   win(1, 1, 80, 25, WHITE, BLACK); clrscr();
   printf("Attestation Near-Earth Satellilte by GLONASS V - 2.0.            ");
      textcolor(LIGHTBLUE);                clreol();
      printf("%s\n\r", flag);              textcolor(LIGHTGRAY);
   printf("Kashchenko V.S, Kharkov Aviation Institute, 1995, Ukraine.      ");
      textcolor(YELLOW);                   clreol();
      printf("%s\n\r", flag);              textcolor(LIGHTGRAY);
   i=printf("  Program work time is  %02i:%02i:%02i.\n\r", (int)Run_Time/3600,
       (int)(Run_Time-Run_Time/3600*3600)/60, (int)(Run_Time-Run_Time/60*60));
   return 1;
}
