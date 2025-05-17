#include "forth.h"

#include "../../../drivers/debug.h"
#include "../../shell.h"
#include "../../terminal.h"
#include "../../../drivers/vga.h"
#include "../../../drivers/vbe.h"
#include "../../../drivers/key.h"
#include "../../../memory/kmalloc.h"


 u32 text_color=TC_WHITE;	///< Текущий цвет шрифта

#define STACK_SIZE 4000 /* cells reserved for the stack */
#define RSTACK_SIZE 4000 /* cells reserved for the return stack */
#define HERE_SIZE1 0x100000

typedef long unsigned Cell;
typedef long sCell;
typedef  void (*proc)(void);

#define pp(cName) const sCell p##cName = (sCell)cName;
#define PP(cName) const sCell P##cName = (sCell)cName;

static int forth_run;
sCell * HereArea;
sCell * StackArea;
sCell * RStackArea;
sCell * here;
sCell *Stack;
sCell *rStack;
sCell * ip ;
sCell ireg;
sCell Tos;
sCell * Handler = NULL;

Cell numericBase = 10;

void dprinthex(sCell value) {
    if (value < 0) {
        dprint("-");
        value = -value;
    }

    if (value == 0) {
        dprint("0");
        return;
    }

    char buffer[21];
    int len = 0;
    while (value > 0) {
        buffer[len++] =
 (value % 16) < 10 ? (value % 16) + '0' : (value % 16) - 10 + 'a' ;
        value /= 16;
    }
    for (int i = len - 1; i >= 0; i--) {
        dprintchar(buffer[i]);
    }
}

volatile int DebagMode=0;
 
void Co( sCell cod ){ *here++ =  cod ;}
void Co2( sCell cod1,sCell cod2 ){Co(cod1); Co(cod2); }
void Co3( sCell cod1,sCell cod2,sCell cod3 ){Co2(cod1,cod2); ; Co(cod3); }
void Co4( sCell cod1,sCell cod2,sCell cod3,sCell cod4 ){  Co3(cod1,cod2,cod3); ; Co(cod4); }
void Co5( sCell cod1,sCell cod2,sCell cod3,sCell cod4,sCell cod5 )
{ Co4(cod1,cod2,cod3,cod4); ; Co(cod5); }
void Co6( sCell cod1,sCell cod2,sCell cod3,sCell cod4,sCell cod5,sCell cod6 )
{ Co5(cod1,cod2,cod3,cod4,cod5); Co(cod6); }
void Co7( sCell cod1,sCell cod2,sCell cod3,sCell cod4,sCell cod5,sCell cod6,sCell cod7 )
{ Co6(cod1,cod2,cod3,cod4,cod5,cod6);  Co(cod7);}

void  Noop(void) {}  pp(Noop)

void  DoVar(void){   *--Stack= Tos; Tos =(sCell)ip; ip = (sCell*)*rStack++; } pp(DoVar)
void  DoConst(void){  *--Stack= Tos; Tos = *ip; ip = (sCell*)*rStack++; } pp(DoConst)
void Execute()
    {
   ireg =Tos; Tos=*Stack++;
	if ( (ireg<0) ^ (pNoop<0) ) {
	((proc) (~ireg))();
	if(DebagMode){dprinthex((sCell)ip); dprintln("<Execute>");}
	return;
    	}
    *--rStack = (sCell) ip;
        ip = (sCell *) ireg;
	if(DebagMode){dprinthex((sCell)ip); dprintln("[Execute]");}
    } pp(Execute)

void DoDefer(){
    ireg = *ip;
    if ( (ireg<0) ^ (pNoop<0) ) {
        ip = (sCell*)*rStack++; // exit
        ((proc)(~ireg))();
	if(DebagMode){dprinthex((sCell)ip); dprintln("<DoDefer>");}
            return;
        }
         ip =  (sCell *) ireg;
	if(DebagMode){dprinthex((sCell)ip); dprintln("<DoDefer>");}
} pp(DoDefer)

void Lit_(){  *--Stack = Tos; Tos = *ip++; } pp(Lit_)
void Lit( sCell val) {Co(~pLit_);  *here++ = val;  }
void Compile(){  *here++ = Tos; Tos = *Stack++;  }  pp(Compile)
void LitCo(){  Co(~pLit_); Compile();}  pp(LitCo)
void Allot(){ *(sCell*)&here += Tos; Tos = *Stack++;  }  pp(Allot)

void  Exit() {
 ip = (sCell*)*rStack++;
	if(DebagMode){dprinthex((sCell)ip); dprintln("<Exit>");}
 } pp(Exit)
void  Here(void){  *--Stack = Tos; Tos = (sCell)here;  } pp(Here)

void Branch(){
 ip = *(sCell**)ip;
	if(DebagMode){dprinthex((sCell)ip); dprintln("<Branch>");}
 } pp(Branch)

void QBranch(){
    if(Tos) ip++;
    else    ip = *(sCell**)ip;
    Tos =   *Stack++;
	if(DebagMode){dprinthex((sCell)ip); dprintln("<QBranch>");}
} pp(QBranch)

void Str() {
    *--Stack = Tos;
    *--Stack = (Cell)ip+1;
        Tos = *(char unsigned *)ip;
    ip = (sCell*) ((Cell)ip + Tos + 1);
    ip = (sCell*) ( ( (Cell)ip + sizeof(Cell) - 1 ) & (-sizeof(Cell) )  );

}  pp(Str)

void Dup(){   *--Stack= Tos;  } pp(Dup)
void Drop(){  Tos = *Stack++;  } pp(Drop)
void Nip(){   Stack++;   } pp(Nip)
void QDup(){   if(Tos) *--Stack= Tos;   } pp(QDup)
void Over(){   *--Stack= Tos; Tos = Stack[1];    } pp(Over)
void Tuck(){    sCell tt=*Stack; *Stack=Tos; *--Stack=tt;  }  pp(Tuck)
void Pick(){    Tos = Stack[Tos];  }  pp(Pick)
void i2dup(){   *--Stack= Tos; *--Stack= Stack[1];  } pp(i2dup)
void i2over(){   *--Stack= Tos;  *--Stack= Stack[3]; Tos= Stack[3];  } pp(i2over)
void i2drop(){  Stack++; Tos = *Stack++; } pp(i2drop)
void Swap(){  sCell tt=Tos; Tos=Stack[0]; Stack[0]=tt;  }  pp(Swap)
void i2Swap(){
    sCell   tt=Tos; Tos=Stack[1]; Stack[1]=tt;
            tt=Stack[0]; Stack[0]=Stack[2]; Stack[2]=tt;  }  pp(i2Swap)
void Rot(){ Cell tt=Stack[1]; Stack[1]=Stack[0]; Stack[0]=Tos; Tos=tt; } pp(Rot)
void Add(){ Tos += *Stack++;  } pp(Add)
void Sub(){ Tos = -Tos;  Tos += *Stack++; } pp(Sub)
void Negate(){ Tos = -Tos; } pp(Negate)
void Invert(){ Tos = ~Tos; } pp(Invert)
void i1Add(){ Tos++; } pp(i1Add)
void i1Sub(){ Tos--; } pp(i1Sub)
void i2Add(){ Tos +=2; } pp(i2Add)
void i2Sub(){ Tos -=2; } pp(i2Sub)
void Mul(){ Tos *= *Stack++; } pp(Mul)
void Div(){ sCell tt=*Stack++; Tos = tt/Tos; } pp(Div)
void i2Mul(){ Tos *= 2; } pp(i2Mul)
void i2Div(){ Tos /= 2; } pp(i2Div)
void Mod(){ sCell tt=*Stack++; Tos = tt%Tos; } pp(Mod)
void UMul(){ Tos = (Cell) Tos * (Cell) *Stack++; } pp(UMul)
void UDiv(){ Cell tt=*Stack++; Tos = tt/(Cell)Tos; } pp(UDiv)
void And(){ Tos &= *Stack++; } pp(And)
void AndC(){ Tos = ~Tos & *Stack++; } pp(AndC)
void Or(){  Tos |= *Stack++; } pp(Or)
void Xor(){ Tos ^= *Stack++; } pp(Xor)
void ARshift(){  Tos = *Stack++ >> Tos ; } pp(ARshift)
void Rshift(){  Tos = *(Cell*)Stack++ >> Tos ; } pp(Rshift)
void Lshift(){  Tos = *Stack++ << Tos ; } pp(Lshift)

void HDot(){
    dprintf("%p ",Tos);
    Tos = *Stack++;
}   pp(HDot)

void UDot() { // itoa( !!!!
    u8 buffer [44];
    u8* p = &buffer;

    size_t s = Tos;

    do {
        ++p;
        s = s / numericBase;
    } while(s);

    *p = ' ';
    p[1] = '\0';

    do {
	u8 nn= (Tos % numericBase);
	if(nn<10)  *--p = '0' + nn;
	else  *--p = 'A' + nn - 10 ;
        Tos = Tos / numericBase;
    } while(Tos);
    term_write(&buffer, text_color);
    Tos = *Stack++; 
}   pp(UDot)

void Dot() {
 if(Tos<0){term_putchar('-', text_color); Negate();} 
 UDot();
}   pp(Dot)

void Load(){  Tos =  *(Cell*)Tos;  } pp(Load)
void Store(){ *(Cell*)Tos = *Stack++;  Tos = *Stack++;} pp(Store)
void CLoad(){ Tos =  (Cell)*(u8*) Tos;  } pp(CLoad)
void CStore(){ *(u8*)Tos = (u8)*Stack++; Tos = *Stack++;  } pp(CStore)
void CAddStore(){ *(u8*)Tos += (u8)*Stack++; Tos = *Stack++;  } pp(CAddStore)
void CStoreA(){ *(u8*)Tos = (u8)*Stack++; } pp(CStoreA)
void WLoad(){ Tos =  (Cell)*(u16*) Tos;  } pp(WLoad)
void WStore(){ *(u16*)Tos = (u16)*Stack++; Tos = *Stack++;  } pp(WStore)

void i2Store(){
 u64 val = ((u64)(Cell)Stack[1]<<32) + (u64)(Cell)Stack[0];
   *(u64*)Tos = val;
   Stack += 2 ;  Tos = *Stack++;} pp(i2Store)

void i2Load(){  u64 val = *(u64*)Tos; Tos= val; *--Stack=val>>32;} pp(i2Load)

void AddStore(){ *(Cell*)Tos += *Stack++;  Tos = *Stack++;} pp(AddStore)
void Count(){ *--Stack = Tos+1; Tos = (sCell) *(char *)Tos; } pp(Count)
void On(){  *(Cell*)Tos = -1; Tos = *Stack++; } pp(On)
void Off(){ *(Cell*)Tos = 0; Tos = *Stack++;  } pp(Off)
void Incr(){  *(Cell*)Tos += 1; Tos = *Stack++; } pp(Incr)
void ZEqual(){ Tos = -(Tos==0); } pp(ZEqual)
void ZNEqual(){ Tos = -(Tos!=0); } pp(ZNEqual)
void DZEqual(){  Tos = -( (Tos | *Stack++) == 0); } pp(DZEqual)
void ZLess(){ Tos = -(Tos<0); } pp(ZLess)
void Equal(){  Tos = -(*Stack++==Tos); } pp(Equal)
void NEqual(){  Tos = -(*Stack++!=Tos); } pp(NEqual)
void Less(){   Tos = -(*Stack++<Tos);  } pp(Less)
void Great(){  Tos = -(*Stack++>Tos);  } pp(Great)
void ULess(){  Tos = -((Cell)*Stack++ < (Cell)Tos); } pp(ULess)
void UGreat(){ Tos = -((Cell)*Stack++ > (Cell)Tos); } pp(UGreat)

void Max(){ sCell tt = *Stack++; if(tt>Tos) Tos=tt; } pp(Max)
void Min(){ sCell tt = *Stack++; if(tt<Tos) Tos=tt; } pp(Min)
void i0Max(){  if(Tos<0) Tos=0; } pp(i0Max)

void ToR(){   *--rStack = Tos; Tos = *Stack++; }	pp(ToR)
void RLoad(){ *--Stack = Tos; Tos = *rStack; }		pp(RLoad)
void FromR(){ *--Stack = Tos; Tos = *rStack++; }	pp(FromR)
void i2ToR(){  *--rStack = *Stack++; *--rStack = Tos ; Tos = *Stack++; } pp(i2ToR)
void i2RLoad(){ *--Stack = Tos; Tos = *rStack; *--Stack = rStack[1];	  } pp(i2RLoad)
void i2FromR(){ *--Stack = Tos; Tos = *rStack++; *--Stack = *rStack++;	  } pp(i2FromR)
void RDrop(){ *rStack++; }    pp(RDrop)
void RPGet(){ *--Stack = Tos; Tos = (Cell) rStack; } pp(RPGet)
void SPGet(){ *--Stack = Tos; Tos = (Cell) Stack ; } pp(SPGet)
void RPSet(){   rStack = (sCell*)Tos; Tos = *Stack++; } pp(RPSet)
void SPSet(){    Stack = (sCell*)(Tos+sizeof(sCell)); Tos = Stack[-1]; } pp(SPSet)

void ZCount(){ *--Stack= Tos; Tos = strlen((char *)Tos); } pp(ZCount)

void Emit() {
 term_putchar((u8)Tos, text_color);
 Tos = *Stack++; } pp(Emit)

void Space() { term_putchar(' ', text_color); } pp(Space)
void Cr() { term_putchar('\n', text_color); } pp(Cr)
void ZType() { term_write((u8*)Tos, text_color); Tos = *Stack++;} pp(ZType)

void Type() {
	char * str = (char *) *Stack;

    for (size_t i = 0; i < Tos; i++) {
	term_putchar_no_cursor_update(str[i], text_color);
    } // punch();
	s_term_info.move_cursor(s_term_info.col, s_term_info.row);
	*Stack++;
    Tos =  *Stack++;
} pp(Type)

void DbgZType() { dprint((u8*)Tos); Tos = *Stack++;} pp(DbgZType)
void DbgZTypeCr() { dprintln((u8*)Tos); Tos = *Stack++;} pp(DbgZTypeCr)
void DbgHex() { dprint("Hex"); dprinthex(Tos); dprint(" "); Tos = *Stack++;} pp(DbgHex)
void Dbg0() { dprint("<Dbg0>");} pp(Dbg0)
void Dbg1() { dprint("<Dbg1>");} pp(Dbg1)
void Dbg2() { dprint("<Dbg2>");} pp(Dbg2)
void Dbg3() { dprint("<Dbg3>");} pp(Dbg3)
void Dbg4() { dprint("<Dbg4>");} pp(Dbg4)
void Dbg5() { dprint("<Dbg5>");} pp(Dbg5)

void SetXY() // ( x y -- )
{ s_term_info.move_cursor(Tos, *Stack++);
	Tos = *Stack++;
} pp(SetXY)

void GetXY() // ( -- x y )
{
    u16 pos ;

    port_byte_out(0x3D4, 0x0F);
    pos = port_byte_in(0x3D5);
    port_byte_out(0x3D4, 0x0E);
    pos |= port_byte_in(0x3D5)  >> 8 ;

	*--Stack = pos%VGA_width;
	Tos = pos/VGA_width;
} pp(GetXY)

void Ahead(){ Co(~pBranch); *--Stack = Tos; Tos = (sCell)here; Co(0);} pp(Ahead)
void If(){ Co(~pQBranch); *--Stack = Tos; Tos= (sCell)here; Co(0);} pp(If)
void Then(){  *(sCell**)Tos++ = here; Tos = *Stack++; } pp(Then)
void Else(){  Ahead();    Swap(); Then(); } pp(Else)
void Begin(){ *--Stack = Tos; Tos =  (sCell)here; } pp(Begin)
void Until(){ Co(~pQBranch);   *here++ = (sCell)Tos; Tos = *Stack++; } pp(Until)
void Again(){ Co(~pBranch);  *here++ = (sCell)Tos; Tos = *Stack++; } pp(Again)
void While(){ If(); Swap(); } pp(While)
void Repeat(){ Again(); Then(); }   pp(Repeat)

void DNegate(){ u64 val =
 -(u64)( ((u64)(Cell)Tos<<32) + (u64)(Cell)Stack[0] ) ;
	Tos= val>>32;
	Stack[0]=val;
  } pp(DNegate)

void DAbs(){   if(Tos<0) DNegate();  } pp(DAbs)

void DAdd()
{ u64 sum= ((u64)(Cell)Tos<<32) + (u64)(Cell)Stack[0] +
	 ((u64)(Cell)Stack[1]<<32) + (u64)(Cell)Stack[2];
	Stack += 2 ;
	Tos= sum>>32;
	Stack[0]=sum;
} pp(DAdd)

void UMMul()
{ u64 mul= (u64)(Cell)Tos * (u64)(Cell)Stack[0] ;
	Tos= mul>>32;
	Stack[0]=mul;
} pp(UMMul)

/* Divide 64-bit unsigned number (high half *b, low half *c) by
   32-bit unsigend number in *a. Quotient in *b, remainder in *c.
*/
static void udiv(u32 a,u32 *b,u32 *c)
{
 u32 d,qh,ql;
 int i,cy;
 qh=*b;ql=*c;d=a;
 if(qh==0) {
  *b=ql/d;
  *c=ql%d;
 } else {
  for(i=0;i<32;i++) {
   cy=qh&0x80000000;
   qh<<=1;
   if(ql&0x80000000)qh++;
   ql<<=1;
   if(qh>=d||cy) {
    qh-=d;
    ql++;
    cy=0;
   }
   *c=qh;
   *b=ql;
  }
 }
}

void UMMOD()
{	if(Tos<=*Stack) { /*overflow */
	*++Stack=-1;
	 Tos = -1; return;
        }
        udiv(Tos,Stack,&Stack[1]);
        Tos = *Stack++;
} pp(UMMOD)

void DIVMOD() // n1 n2 -- rem quot
{	sCell tt=*Stack;
        *Stack = tt%Tos ;
	Tos = tt/Tos;
} pp(DIVMOD)

void Align()
{   Cell sz = ( sizeof (Cell) - 1 ) ;
    char * chere = (char *)here;
    while( (Cell) chere & sz ) *chere++ = 0 ;
    here = (sCell *)chere;
}

// CODE FILL ( c-addr u char -- ) \ 94
void Fill()
{    Cell len =  *Stack++;
    u8 *adr = (u8 *) *Stack++;
  while (len-- > 0)  *adr++ = (u8)Tos;
  Tos =  *Stack++;
}  pp(Fill)

void Cmove()
{
  u8 *c_to = (u8 *) *Stack++;
  u8 *c_from =(u8 *) *Stack++;
  while (Tos-- > 0)
    *c_to++ = *c_from++;
  Tos =  *Stack++;
}  pp(Cmove)

void Cmove_up()
{
  u8 *c_to = (u8 *) *Stack++;
  u8 *c_from =(u8 *) *Stack++;
  while (Tos-- > 0)
    c_to[Tos] = c_from[Tos];
  Tos =  *Stack++;
}  pp(Cmove_up)

void StrComp(const char * s, sCell len)
{   char * chere = (char *)here;
    len &= 0xff ;
    *chere++ = (char)len;                /* store count byte */
    while (--len >= 0)          /* store string */
        *chere++ = *s++;

    here = (sCell *)chere;
    Align();
}

void StrCmp(){  StrComp((char *) *Stack++, Tos); Tos = *Stack++; } pp(StrCmp)

void Tp(const char * s) {
    Co(~pStr);
    StrComp(s, strlen(s));
    Co(~pType);
}

void SpSet(){    Stack = (sCell*)*Stack; } pp(SpSet)

sCell  ForthWordlist[] = {0,0,0};

#define ContextSize 10

sCell * Context[ContextSize] = {ForthWordlist};
sCell * Current[] = {ForthWordlist};

sCell * Last;
sCell * LastCFA;

void  WordBuild (const char * name, sCell cfa )
{
    LastCFA=here;
    Co(cfa);
    Co(0); // flg
    Co(** (sCell **) Current);
    Last=here;
    StrComp(name, strlen(name));
}

void Smudge(){ **(sCell***) Current=Last; } pp(Smudge)

void Immediate(){ Last[-2] |= 1; } pp(Immediate)

void FthItem (const char * name, sCell cfa ){
    WordBuild (name, cfa );
    Smudge();
}

sCell Header(const char * name) {
    FthItem (name,0);
    *(sCell **)LastCFA = here;
    return  *(sCell *)LastCFA;
}

sCell Variable (const char * name ) {
    FthItem(name,0);
    *(sCell **) LastCFA = here;
    *here++ =  pDoVar;
    *here++ = 0;
    return  *(sCell *)LastCFA;
}

sCell VVariable (const char * name, sCell val ) {
    FthItem(name,0);
    *(sCell **) LastCFA = here;
    *here++ = ~pDoVar;
    *here++ = val;
    return  *(sCell *)LastCFA;
}

sCell Constant (const char * name, sCell val ) {
    FthItem(name,0);
    *(sCell **) LastCFA = here;
    *here++ = ~pDoConst;
    *here++ = val;
    return  *(sCell *)LastCFA;
}
char atib[256]={"atib atib qwerty"};
sCell tib[]={0,(sCell)&atib}; PP(tib)
sCell ntib;
void Source(){
 *--Stack = Tos;
 *--Stack = tib[1];
    Tos =  ntib;
  } pp(Source)

void SourceSet(){
  ntib = Tos;
 tib[1] = *Stack++;
 Tos = *Stack++;
  } pp(SourceSet)

// ALLOCATE ( u -- a-addr ior ) 
void Allocate()
{
	*--Stack= Tos;
	
	*Stack= (sCell) kmalloc(Tos);
	Tos=0;
  	if(*Stack==0) Tos=-59;

} pp(Allocate)

void Free()
{
	kfree(Tos);
	Tos=0;

} pp(Free)

sCell i2in[] = {0 , 0  }; PP(i2in)
sCell *v2in = (sCell *) &i2in[1];

sCell SourceId[] = { 0, 0 }; PP(SourceId)

void Accept() // ( c-addr +n -- +n' )
{
    char * command_buffer = (char *)*(Stack++);
    unsigned command_length = 0;

    for (;;) {
        key_event_t event;
        ps2_get_key_event(&event);

        /* Halt the CPU until an interrupt if there is no input */
        if (event.key == KEY_NONE) {
            asm volatile("hlt");
            continue;
        }

        /* Discard key release events */
        if (event.modifiers & KEY_EVENT_MODIFIERS_RELEASED) {
            continue;
        }
        switch (event.key) {
            case KEY_Backspace:
                if (command_length > 0) {
                    term_write("\b \b", TC_WHITE);
                    command_length--;
                }
                break;
            case KEY_Enter:
           command_buffer[command_length] = '\0';
	add_command_to_history(command_buffer);
	 Tos = command_length;
		return;
            case KEY_ArrowDown:
                if (cmd_history.history_count > 0) {
                    if (cmd_history.history_position >= 0) {
                        cmd_history.history_position++;
                        if (cmd_history.history_position >= cmd_history.history_count) {
                            // If we exceed the history, reset to a blank command line
                            cmd_history.history_position = -1;
                        }

                        // Clear the current input
                        while (command_length > 0) {
                            term_write("\b \b", TC_WHITE);
                            command_length--;
                        }

                        // If we're not at the blank command line, copy the history command to the buffer
                        if (cmd_history.history_position >= 0) {
                            strcpy(command_buffer, cmd_history.commands[cmd_history.history_position]);
                            command_length = strlen(command_buffer);
                            term_write(command_buffer, TC_WHITE);
                        }
                    }
                }
                break;
            case KEY_ArrowUp:
                if (cmd_history.history_count > 0) {
                    if (cmd_history.history_position < 0) {
                        cmd_history.history_position = cmd_history.current_index - 1;
                    } else {
                        cmd_history.history_position--;
                        if (cmd_history.history_position < 0) {
                            cmd_history.history_position = cmd_history.history_count - 1;
                        }
                    }

                    // Clear the current input
                    while (command_length > 0) {
                        term_write("\b \b", TC_WHITE);
                        command_length--;
                    }

                    // Copy the history command to the buffer
                    strcpy(command_buffer, cmd_history.commands[cmd_history.history_position]);
                    command_length = strlen(command_buffer);
                    term_write(command_buffer, TC_WHITE);
                }
                break;
            case KEY_Tab:
                if (command_length+4>=sizeof(command_buffer)){
                    //don't try to add the tab if the buffer doesn't have enough space
                    break;
                }
                if (command_length > 0){
                    for (int i=0;i<4;++i){
                        term_write(" ",TC_WHITE);
                        command_buffer[command_length] = ' ';
                        command_length++;

                    }
                }
                break;
            default: {
                const char* utf8 = key_to_utf8(&event);

                while (utf8 && *utf8) {
                    if (command_length >= Tos - 1) {
                        break;
                    }
                    term_putchar(*utf8, TC_WHITE);
                    command_buffer[command_length++] = *utf8;
                    utf8++;
                }
                break;
            }
        }
    }
} pp(Accept)

void ParseName() {
    Cell addr,Waddr,Eaddr;
    addr=  tib[1] + *v2in;
    Eaddr= tib[1] + ntib;

    *--Stack = Tos;
    while (  addr<Eaddr ) { if( *(u8*)addr > ' ') break;
        addr++; }
    *--Stack=Waddr=addr;
    *v2in = addr - tib[1];
    while ( addr<=Eaddr ) { (*v2in)++; if( *(u8*)addr <= ' ') break;
     addr++; }
    Tos=addr-Waddr;
} pp(ParseName)

void Parse() {
    Cell addr,Waddr,Eaddr;
	if(((u8*)tib[1])[ntib] == '\r' ) ntib--;
    addr=  tib[1]  + *v2in;
    Eaddr= tib[1]  + ntib;

    char cc = (char)Tos;
    *--Stack=Waddr=addr;
    while ( addr<=Eaddr ) {  (*v2in)++;  if(*(u8*)addr == cc ) break;
        addr++;}
    Tos=addr-Waddr;
} pp(Parse)

#ifndef memcasecmp

Cell memcasecmp (const void *vs1, const void *vs2, Cell n)
{
    unsigned int i;
    u8 const *s1 = (u8 const *) vs1;
    u8 const *s2 = (u8 const *) vs2;
    for (i = 0; i < n; i++)
    {
        u8 u1 = *s1++;
        u8 u2 = *s2++;
        if (toupper (u1) != toupper (u2))
            return toupper (u1) - toupper (u2);
    }
    return 0;
}
#endif

Cell CCompare( void * caddr1  ,  Cell len1 ,  void * caddr2  ,  Cell len2) {
    if (len1 < len2) return -1;
    if (len1 > len2) return  1;

//    auto cmpResult = std::memcmp(caddr1, caddr2, len1);
    auto cmpResult = memcasecmp(caddr1, caddr2, len1);

    if (cmpResult < 0) return -1;
    if (cmpResult > 0) return  1;
    return   0;
}

void UCompare(){ 
	char * caddr1 = (char *) *Stack++;
	sCell  len1 =  *Stack++;
	char * caddr2 = (char *) *Stack++;

    if (len1 != Tos) {  Tos -= len1; return; }

    Tos = memcasecmp(caddr1, caddr2, Tos);  } pp(UCompare)

char *SEARCH(char **wid,  char * word , Cell len)
{ char * addr= (char *) *wid;
    for(;;)
    {   if(!addr) return NULL;
        char * caddr = addr ;
        if( !CCompare(word, len, caddr+1, *caddr ))
            return  addr;
        addr = ((char **)addr)[-1];
    }
}

void FromName(){  Tos=((sCell *)Tos)[-3]; } pp(FromName)

void SearchWordList() // ( c-addr u wid --- 0 | xt 1 xt -1 )
{
    char ** addr=  (char **) Tos;
    Cell  len=Stack[0];
    char * word= (char * ) Stack[1];

    if(!addr) { Stack+=2; Tos=0; return; }
    Cell * nfa= (Cell*) SEARCH(addr,word,len);
    if(!nfa) {
        Stack+=2; Tos=0;
        return;
    }
    Stack++;
    Stack[0]=nfa[-3];
    Tos = nfa[-2]&1 ? 1 : -1;

}  pp(SearchWordList)

void SFind()
{	sCell * voc=  (sCell *) Context;
    *--Stack = Tos;
    while( *voc )
    {	*--Stack = Stack[1];
        *--Stack = Stack[1]; Tos=*voc;
        SearchWordList();
        if(Tos)
        {   Stack[2]=Stack[0];  Stack+=2; // 2nip
            return;
        }   voc++;
    }

} pp(SFind)

Cell State;

void StateQ(){ *--Stack= Tos; Tos = State; } pp(StateQ)

void IMode(){ State = 0;}  pp(IMode)
void CMode(){ State = -1;}  pp(CMode)

sCell * YDP;
sCell * YDP0;

sCell YDPFL[] = { pDoConst, 0 }; pp(YDPFL)

void QYDpDp()
{
  if(YDPFL[1] == 0) return;
   sCell * tmp = YDP ;
    YDP = here ;
    here = tmp ;
}

void SBuild()
{    char * name = (char * ) *Stack++ ;
	QYDpDp();
    LastCFA=here;
    Co(0);
    Co(0); // flg
    Co(** (sCell  **) Current);
    Last=here;
    StrComp(name, Tos);
    Tos = *Stack++;
	QYDpDp();
    *(sCell **)LastCFA = here;
}

void Build()
{ //   *--Stack = Tos; Tos=(sCell)pNoop;
    ParseName();
    SBuild();
} pp(Build)

void SHeader()
{
	SBuild();
	Smudge();  
} pp(SHeader)


unsigned long strtoul(const char* str, char** endptr, int base) {
    unsigned long result = 0;
    int i = 0;

    // Пропускаем начальные пробелы
    while (str[i] == ' ') {
        i++;
    }

    // Преобразуем основание base в случае необходимости
    if (base == 0) {
        if (str[i] == '0') {
            if (str[i + 1] == 'x' || str[i + 1] == 'X') {
                base = 16;
                i += 2;
            } else {
                base = 8;
                i++;
            }
        } else {
            base = 10;
        }
    }

    // Обрабатываем число
    while (str[i] != '\0') {
        int digit;
        if (str[i] >= '0' && str[i] <= '9') {
            digit = str[i] - '0';
        } else if (str[i] >= 'a' && str[i] <= 'z') {
            digit = str[i] - 'a' + 10;
        } else if (str[i] >= 'A' && str[i] <= 'Z') {
            digit = str[i] - 'A' + 10;
        } else {
            break;
        }
        if (digit < base) {
            result = result * base + digit;
            i++;
        } else {
            break;
        }
    }

    // Устанавливаем значение endptr
    if (endptr != NULL) {
        *endptr = (char*)(str + i);
    }

    return result;
}

void SNumber0() // ( str len -- m flg )
{
    char* rez;
    char  NumStr[44];
    sCell signedFlg = 1;
    Cell len = Tos;
    char * caddr = (char*) Stack[0];
    if(caddr[0]=='-') { len--; caddr++; signedFlg = -1; }
    NumStr[len]=0;
    while(len){ --len; NumStr[len] = caddr[len]; }
    *Stack = strtoul( NumStr,  &rez, numericBase) * signedFlg;
    Tos =  strlen(rez);
}  pp(SNumber0)

void Colon(){
  Build();
  CMode(); } pp( Colon)
void Semicolon(){ Co(~pExit); Smudge(); IMode(); } pp(Semicolon)

void to_catch(){
    *--rStack = (sCell)Handler;
    *--rStack = (sCell)Stack;
    Handler = rStack;
    Execute();
} pp(to_catch)

void from_catch(){
    rStack++;
    Handler = (sCell*)*rStack++;
    *--Stack = Tos;  Tos = 0;
    ip = (sCell*)*rStack++; // exit
} pp(from_catch)

sCell Catch[] = { 0,0 }; PP(Catch)

void FThrowDo()
{   *--Stack = Tos;
//    if (Handler == NULL); //  TODO("Handler=0")
    rStack =   Handler ;
    Stack = (sCell*)*rStack++;
    Handler = (sCell*)*rStack++;
    ip = (sCell * ) *rStack++;
}

void FThrow(){
    if (Tos == 0){  Tos = *Stack++; return;  }
    FThrowDo();
} pp(FThrow)

sCell Lastin =0;
sCell SaveErrQ = -1;
sCell ErrIn;

void SaveErr0()
{ if(SaveErrQ & Tos )
    {  SaveErrQ = 0;
       ErrIn = *v2in ;
    }

} pp(SaveErr0)


void PrintErr0()
{  numericBase = 10;
     dprintf("Err=%d\n",Tos);
     Tos = *Stack++;
     SaveErrQ=-1;
} pp(PrintErr0)

// R/O ( -- fam )
void readOnly() { *--Stack = Tos; Tos = 1; }  pp(readOnly)

// R/W ( -- fam )
void readWrite() { *--Stack = Tos; Tos = 3; } pp(readWrite)

// W/O ( -- fam )
void writeOnly() { *--Stack = Tos; Tos = 2 ; } pp(writeOnly)

void openFile() {
	FAT_PFile_t * file = kmalloc(sizeof(FAT_PFile_t));
	dprint("openFile=");
	char FName[512];
    Cell flen = *Stack++;
    Cell plen = 0;
    char * caddr = (char*) *Stack;
	dprintln(caddr);


	for(;plen<flen;plen++)	FName[plen]=caddr[plen];
	FName[plen]=0;
	dprint("FName="); dprintln(&FName);

    file->file = FAT_OpenAbsolute(s_fat_fs, &FName);
    if (file->file == NULL) {
	kfree(file);
	Tos = -69;
	return ;
	}

    file->pos = 0;
    *Stack = (sCell)file;
    Tos = 0;
} pp(openFile)

// CLOSE-FILE ( fileid -- ior )
void closeFile() {
	FAT_PFile_t * file = (FAT_PFile_t*)Tos;
	FAT_Close(file->file);
	kfree(file);
	Tos = 0; } pp(closeFile)


// READ-FILE ( c-addr u1 fileid -- u2 ior )
void readFile() {
	
    Cell len = *Stack++;
    char * buffer = (char*) *Stack;
    FAT_PFile_t* file = (FAT_PFile_t*) Tos;
    size_t total_read = 0;

    *Stack = FAT_Read(file->file, file->pos, buffer, len);
	file->pos+=*Stack;
// fread( file , 1, len, buffer);
	Tos = 0;
	if(*Stack==-1) Tos = -70; 

} pp(readFile)

// READ-LINE ( c-addr u1 fileid -- u2 flag ior )
void readLine() {

	FAT_PFile_t* file = (FAT_PFile_t*) Tos;

	if(file->pos >= file->file->file_size){ Stack[1]=*Stack=Tos=0; return;}
	
	char * buffer = (char*) Stack[1];
//	if( len > (file->file->file_size+1 - file->pos))
//	 len = file->file->file_size+1 - file->pos;

	dprint("FAT_Read:");
	dprinthex(*Stack);dprint(":");
	Cell lenMax = FAT_Read(file->file, file->pos, buffer, *Stack);
//	file->pos+=*Stack;
	dprinthex(lenMax);dprint("|");
 // dprintln(buffer);
//	Tos = 0;
	if(lenMax>(Cell)*Stack){ dprint(buffer);  Tos = -71; return; }
//	file->pos -= len;
    	Cell len = 0;
	while(lenMax > len)
	{  file->pos++;
	 if(buffer[len]=='\n'){ break;}
	len++;
	}
	dprinthex(len);dprint(":");

	dprinthex(file->pos);dprintln(".");
     	if(buffer[len]=='\r') len--;
	Stack[1]=len;
	*Stack=-1;
	Tos=0;

} pp(readLine)

// WRITE-FILE ( c-addr u1 fileid -- ior )
void writeFile() {

    Cell len = *Stack++;
    char * buffer = (char*) *Stack;
    FAT_PFile_t* file = (FAT_PFile_t*) Tos;
    size_t total_read = 0;

    Tos = FAT_Write(file->file, file->pos, buffer, len);
	Tos = 0; // &= -70;

} pp(writeFile)

// RESIZE-FILE ( ud fileid -- ior ) 

void resizeFile() {
    FAT_PFile_t* file = (FAT_PFile_t*) Tos;
   file->file->file_size = *Stack++;
   Tos=0;
} pp(resizeFile)

void  Bye(void) {forth_run=0;}  pp(Bye)


const char *initScript =
        " : 2NIP 2SWAP 2DROP ;\n"
        " : COMPILE, , ;\n"
        " : HEX 16 BASE ! ;\n"
        ": DECIMAL 10 BASE ! ;\n"
        ": HEADER BUILD SMUDGE ;\n"
        ": CONSTANT HEADER DOCONST , , ;\n"
        ": CREATE HEADER DOVAR , ;\n"
        ": VARIABLE CREATE 0 , ;\n"
        ": [COMPILE] ' , ; IMMEDIATE\n"
        ": CELL+ CELL + ;\n"
        ": CELL- CELL - ;\n"
        ": CELLS CELL * ;\n"
        ": >BODY CELL+ ;\n"
        ": COMPILE R> DUP @ , CELL+ >R ;\n"
        ": CHAR  PARSE-NAME DROP C@ ;\n"
        ": [CHAR] CHAR LIT,  ; IMMEDIATE\n"
        ": [']  ' LIT, ; IMMEDIATE\n"
        ": .( [CHAR] ) PARSE TYPE ; IMMEDIATE\n"
        ": ( [CHAR] ) PARSE 2DROP ; IMMEDIATE\n"
        ": SLIT, ( string -- ) COMPILE <$> $, ;\n"
        ": \\ 10 PARSE 2DROP  ; IMMEDIATE\n"
        ": .\\ 10 PARSE TYPE cr ; IMMEDIATE\n"
        ": .\" [CHAR] \" PARSE SLIT, COMPILE TYPE   ; IMMEDIATE\n"
        ": S\" [CHAR] \" PARSE ?STATE IF SLIT, THEN ; IMMEDIATE\n"
        ": ABORT -1 THROW ;\n"
        ": POSTPONE\n" // 94
        "  PARSE-NAME SFIND DUP\n"
        "  0= IF -321 THROW THEN \n"
        "  1 = IF COMPILE,\n"
        "      ELSE LIT, ['] COMPILE, COMPILE, THEN\n"
        "; IMMEDIATE\n"
        ": TO '\n"
        "   ?STATE 0= IF >BODY ! EXIT THEN\n"
        "    >BODY LIT, POSTPONE ! ; IMMEDIATE\n"
	": ERASE 0 FILL ;\n"
	": $!\n" //	( addr len dest -- )
	"SWAP 255 AND SWAP	2DUP C! 1+ SWAP CMOVE ;\n"
        ": DEFER@  ( xt1 -- xt2 )  >BODY @ ;\n"
        ": VALUE CONSTANT ;\n"
        ": (DO)   ( n1 n2 ---)\n"
        // Runtime part of DO.
        " R> ROT ROT SWAP >R >R >R ;\n"
        ": (?DO)  ( n1 n2 ---)\n"
        // Runtime part of ?DO
        "  OVER OVER - IF R> ROT ROT SWAP >R >R CELL+ >R \n"
        "                 ELSE DROP DROP R> @ >R\n" // Jump to leave address if equal
        "                 THEN ;\n"
        ": I ( --- n )\n"
        // Return the counter (index) of the innermost DO LOOP
        "  POSTPONE R@ ; IMMEDIATE\n"
                ": z\\ 10 PARSE h. h. ; IMMEDIATE\n"

        ": J  ( --- n)\n"
        // Return the counter (index) of the next loop outer to the innermost DO LOOP
        " RP@ 3 CELLS + @ ;\n"
        "VARIABLE 'LEAVE ( --- a-addr)\n" // This variable is  used  for  LEAVE address resolution.

        ": (LEAVE)   ( --- )\n"
        // Runtime part of LEAVE
        " R> @ R> DROP R> DROP >R ;\n" // Remove loop parameters and replace top of ret\n"
        // stack by leave address.\n"

        ": UNLOOP ( --- )\n"
        // Remove one set of loop parameters from the return stack.
        "   R> R> DROP R> DROP >R ;\n"

        ": (LOOP) ( ---)\n"
        // Runtime part of LOOP
        "  R> R> 1+ DUP R@ = \n"   // Add 1 to count and compare to limit.
        "  IF \n"
        "   R> DROP DROP CELL+ >R\n" // Discard parameters and skip leave address.
        "  ELSE \n"
        "   >R @ >R\n" // Repush counter and jump to loop start address.
        "  THEN ;\n"

        ": (+LOOP) ( n ---)\n"
        // Runtime part of +LOOP
        // Very similar to (LOOP), but the compare condition is different.
        //  exit if ( oldcount - lim < 0) xor ( newcount - lim < 0).
        "     R> SWAP R> DUP R@ - ROT ROT + DUP R@ - ROT XOR 0 < \n"
        "     IF R> DROP DROP CELL+ >R\n"
        "     ELSE >R @ >R THEN ;\n"

        ": DO ( --- x)\n"
        // Start a DO LOOP.
        // Runtime: ( n1 n2 --- ) start a loop with initial count n2 and
        // limit n1.
        "  POSTPONE (DO) 'LEAVE @  HERE 0 'LEAVE ! \n"
        "   ; IMMEDIATE\n"

        ": ?DO  ( --- x )\n"
        // Start a ?DO LOOP.\n"
        // Runtime: ( n1 n2 --- ) start a loop with initial count n2 and
        // limit n1. Exit immediately if n1 = n2.
        "  POSTPONE (?DO)  'LEAVE @ HERE 'LEAVE ! 0 , HERE ; IMMEDIATE\n"

        ": LEAVE ( --- )\n"
        // Runtime: leave the matching DO LOOP immediately.
        // All places where a leave address for the loop is needed are in a linked\n"
        // list, starting with 'LEAVE variable, the other links in the cells where
        // the leave addresses will come.
        "  POSTPONE (LEAVE) HERE 'LEAVE @ , 'LEAVE ! ; IMMEDIATE\n"
        ": RESOLVE-LEAVE\n"
        // Resolve the references to the leave addresses of the loop.
        "         'LEAVE @\n"
        "         BEGIN DUP WHILE DUP @ HERE ROT ! REPEAT DROP ;\n"

        ": LOOP  ( x --- )\n"
        // End a DO LOOP.
        // Runtime: Add 1 to the count and if it is equal to the limit leave the loop.
        " POSTPONE (LOOP) ,  RESOLVE-LEAVE  'LEAVE ! ; IMMEDIATE\n"

        ": +LOOP  ( x --- )\n"
        // End a DO +LOOP
        // Runtime: ( n ---) Add n to the count and exit if this crosses the
        // boundary between limit-1 and limit.
        " POSTPONE (+LOOP) , RESOLVE-LEAVE 'LEAVE ! ; IMMEDIATE\n"

        ": (;CODE) ( --- )\n"
        // Runtime for DOES>, exit calling definition and make last defined word
        // execute the calling definition after (;CODE)
        "  R> LAST @  NAME>  ! ;\n"

        ": DOES>  ( --- )\n"
        // Word that contains DOES> will change the behavior of the last created
        // word such that it pushes its parameter field address onto the stack
        // and then executes whatever comes after DOES>
        " POSTPONE (;CODE) \n"
        " POSTPONE R>\n" // Compile the R> primitive, which is the first
        // instruction that the defined word performs.
        "; IMMEDIATE\n"

    ": SET-CURRENT ( wid -- )\n" // 94 SEARCH
    "        CURRENT ! ;\n"

    ": GET-CURRENT ( -- wid )\n" // 94 SEARCH
    "        CURRENT @ ;\n"

    ": GET-ORDER ( -- widn ... wid1 n )\n"  // 94 SEARCH
        " SP@ >R 0 >R\n"
        " CONTEXT\n"
        " BEGIN DUP @ ?DUP\n"
        " WHILE >R CELL+\n"
        " REPEAT  DROP\n"
        " BEGIN R> DUP 0=\n"
        " UNTIL DROP\n"
        "R> SP@ - CELL / 1- ; \n"

	" HERE S\" FORTH\" $, FORTH-WORDLIST CELL+ !\n"

        ": VOC-NAME. ( wid -- )\n"
        "DUP CELL+ @ DUP IF COUNT TYPE BL EMIT DROP ELSE DROP .\" <NONAME>:\" U. THEN ;\n"

        ": ORDER ( -- )\n" // 94 SEARCH EXT
        "GET-ORDER .\" Context: \" \n"
        "0 ?DO ( DUP .) VOC-NAME. SPACE LOOP CR\n"
        ".\" Current: \" GET-CURRENT VOC-NAME. CR ;\n"

        ": SET-ORDER ( wid1 ... widn n -- )\n"
        "DUP -1 = IF\n"
        "DROP  FORTH-WORDLIST 1\n"
        "THEN\n"
        "DUP  CONTEXT-SIZE  U> IF -49 THROW THEN\n"
        "DUP CELLS context + 0!\n"
        "0 ?DO I CELLS context + ! LOOP ;\n"
        "CREATE VOC-LIST FORTH-WORDLIST CELL+ CELL+ ,\n"

        ": FORTH FORTH-WORDLIST CONTEXT ! ;\n"
        ": DEFINITIONS  CONTEXT @ CURRENT ! ;\n"

        ": WORDLIST ( -- wid )\n" // 94 SEARCH
        " HERE 0 , 0 , \n"
        " HERE VOC-LIST  @ , .\" W=\" DUP H.  VOC-LIST ! ;\n"

	": ONLY ( -- ) -1 SET-ORDER ;\n"
	": ALSO ( -- )   GET-ORDER OVER SWAP 1+ SET-ORDER ;\n"
	": PREVIOUS ( -- ) GET-ORDER NIP 1- SET-ORDER ;\n"


	": LATEST ( -> NFA ) CURRENT @ @ ;\n"

	": VOCABULARY ( <spaces>name -- )\n"
	"WORDLIST CREATE DUP ,\n"
	"LATEST SWAP CELL+ !\n"
	"DOES>  @ CONTEXT ! ;\n"
	" VARIABLE CURSTR\n"

	": ->DEFER ( cfa <name> -- )  HEADER DODEFER , , ;\n"
	": DEFER ( <name> -- ) ['] ABORT ->DEFER ;\n"

        ": VECT DEFER ;\n"

	": FQUIT BEGIN REFILL WHILE CURSTR 1+!\n"
	"  CR SOURCE TYPE\n"
	"  INTERPRET  REPEAT ;\n"

	": LALIGNED  3 + 3 ANDC ;\n"

	" 255 CONSTANT TC/L\n"

 ": INCLUDE-FILE\n" // ( fid --- )
// Read lines from the file identified by fid and interpret them.
// INCLUDE and EVALUATE nest in arbitrary order.
	" SOURCE-ID >R >IN @ >R LASTIN @ >R CURSTR @ >R CURSTR 0!\n"
	"SOURCE 2>R\n"
	"TC/L ALLOCATE THROW TC/L SOURCE!\n"
	"TO SOURCE-ID\n"
	"['] FQUIT CATCH SAVEERR\n"
	"TIB FREE DROP\n"
	"2R> SOURCE!\n"

	"R> CURSTR ! R> LASTIN ! R> >IN ! R> TO SOURCE-ID\n"
	"THROW ;\n"

	": FREFILL0\n" // (  -- flag )
	" TIB TC/L SOURCE-ID READ-LINE THROW\n"
	" SWAP  #TIB !  0 >IN ! CURSTR 1+!\n"
	" 0 SOURCE + C! ;\n"
	"' FREFILL0 TO FREFILL\n"

  "444 CONSTANT  CFNAME_SIZE\n"
  "CREATE CURFILENAME  CFNAME_SIZE 255 + 1+ ALLOT\n"
  "CURFILENAME  CFNAME_SIZE 255 + 1+  ERASE\n"

  ": CFNAME-SET\n" // ( adr len -- )
  "DUP 1+ >R  CURFILENAME CURFILENAME R@ + CFNAME_SIZE R> - CMOVE>\n"
  "CURFILENAME $! ;\n"

  ": CFNAME-FREE\n" //  ( -- )
  "CURFILENAME COUNT + CURFILENAME\n"
  "CFNAME_SIZE CURFILENAME C@ - 255 +  CMOVE ;\n"

 ": INCLUDED\n" // ( c-addr u ---- )
 "2DUP CFNAME-SET\n"
 "R/O OPEN-FILE .\" op=\" DBG2 DUP DBGH. DUP H. DUP DBGH. THROW\n"
 "DUP >R ['] INCLUDE-FILE CATCH\n"
 "DUP IF cr .\" in <\" CURFILENAME COUNT TYPE .\" >\" CURSTR @ . THEN  CFNAME-FREE\n"
 "R> CLOSE-FILE DROP THROW ;\n"

 ": EVALUATE\n" // ( i*x c-addr u -- j*x ) \ 94
 "SOURCE-ID >R SOURCE 2>R >IN @ >R\n"
 "-1 TO SOURCE-ID\n"
 "SOURCE! >IN 0!\n"
 "['] INTERPRET CATCH\n"
 "R> >IN ! 2R> SOURCE! R> TO SOURCE-ID\n"
 "THROW ;\n"

 ": FLOAD DBG0 PARSE-NAME DBG1 INCLUDED ;\n"

 ": [DEFINED]\n" //  ( -- f ) \ "name"
 "PARSE-NAME  SFIND  IF DROP -1 ELSE 2DROP 0 THEN ; IMMEDIATE\n"

 ": [UNDEFINED]\n" //  ( -- f ) \ "name"
 "POSTPONE [DEFINED] 0= ; IMMEDIATE\n"

 ": \\+	POSTPONE [UNDEFINED]	IF POSTPONE \\ THEN ; IMMEDIATE\n"
 ": \\-	POSTPONE [DEFINED]	IF POSTPONE \\ THEN ; IMMEDIATE\n"

 ": BREAK  POSTPONE EXIT POSTPONE THEN ; IMMEDIATE\n"

 ": PRIM? 0< ['] DUP 0< = ;\n"

 ": ?CONST\n" // ( cfa -- cfa flag )
 "DUP PRIM? IF 0 BREAK\n"
 "DUP @ DOCONST = ;\n"

 ": ?VARIABLE\n" // ( cfa -- cfa flag )
 "DUP PRIM? IF 0 BREAK\n"
 "DUP @ DOVAR = ;\n"
 ": SYS> 10 PARSE SYSCALL ;\n"
 ": ls 0 >IN ! SYS> ;\n"
 ": cd 0 >IN ! SYS> ;\n"
 ": cls 0 >IN ! SYS> ;\n"
 ": pause 0 >IN ! SYS> ;\n"
 ": pl 0 >IN ! SYS> ;\n"
 ": chstat 0 >IN ! SYS> ;\n"
 ": vbetest 0 >IN ! SYS> ;\n"
 ": whereami 0 >IN ! SYS> ;\n"
 ": recovery 0 >IN ! SYS> ;\n"

//  "S\" ForthLib/CompIF4.4th\" R/O OPEN-FILE H. CONSTANT FID\n"

  "S\" autoexec.4th\" INCLUDED\n"
;

void  InitStringSet()
{ 	tib[1]=(u32)initScript;
	ntib=strlen(initScript);
    *v2in = 0;
} pp(InitStringSet)

u8 CKey_t()
{ key_event_t event;

//    term_putchar('|', TC_WHITE);
    term_putchar('|', TC_WHITE);
     do{   ps2_get_key_event(&event);
	} while ( (event.modifiers &
	 KEY_EVENT_MODIFIERS_RELEASED) |
	(event.key==0) ) ;
     const char* utf8 = key_to_utf8(&event);

    term_write_hex(event.key);
     return *utf8;
}

sCell CKey()
{ key_event_t event;
  const char* utf8;
     do{   ps2_get_key_event(&event);
	} while ( (event.modifiers &
	 KEY_EVENT_MODIFIERS_RELEASED) |
	(event.key==0) ) ;

        switch (event.key) {
            case KEY_Backspace: return 8;
            case KEY_Enter:     return 0xd;
            case KEY_Tab:       return 9;
     	 default: utf8 = key_to_utf8(&event);
            }
	if(!utf8) return (event.key&0x100);
     return *utf8;

}

void  KeyQ()
{ *--Stack= Tos;
//  Tos = keyboard->byte_buffer_len;
} pp(KeyQ)

void  Key()
{ *--Stack= Tos;
  Tos = CKey();
} pp(Key)

void  SysCall()
{ parse_command((char*)*Stack++,Tos);
  Tos = *Stack++;
} pp(SysCall)

void  MakeImag(void)
{
    FthItem("NOOP",~pNoop );
    FthItem("+",~pAdd );
    FthItem("-",~pSub );
    FthItem("D+",~pDAdd );
    FthItem("1+",~pi1Add );
    FthItem("1-",~pi1Sub );
    FthItem("2+",~pi2Add );
    FthItem("2-",~pi2Sub );
    FthItem("INVERT",~pInvert);
    FthItem("NEGATE",~pNegate);
    FthItem("DNEGATE",~pDNegate);
    FthItem("DABS",~pDAbs);
    FthItem("*",~pMul);
    FthItem("/",~pDiv);
    FthItem("2*",~pi2Mul);
    FthItem("2/",~pi2Div);
    FthItem("MOD",~pMod);
    FthItem("U*",~pUMul);
    FthItem("U/",~pUDiv);
    FthItem("UM*",~pUMMul);
    FthItem("UM/MOD",~pUMMOD);
    FthItem("/MOD",~pDIVMOD);
    FthItem("AND",~pAnd);
    FthItem("ANDC",~pAndC);
    FthItem("OR",~pOr);
    FthItem("XOR",~pXor);
    FthItem("ARSHIFT",~pARshift);
    FthItem("RSHIFT",~pRshift);
    FthItem("LSHIFT",~pLshift);
    FthItem("DUP",~pDup );
    FthItem("CS-DUP",~pDup );
    FthItem("?DUP",~pQDup );
    FthItem("OVER",~pOver );
    FthItem("CS-OVER",~pOver );
    FthItem("TUCK",~pTuck );
    FthItem("PICK",~pPick );
    FthItem("CS-PICK",~pPick );
    FthItem("SWAP",~pSwap );
    FthItem("CS-SWAP",~pSwap );
    FthItem("2SWAP",~pi2Swap );
    FthItem("ROT",~pRot );
    FthItem("DROP",~pDrop );
    FthItem("NIP",~pNip );
    FthItem("2DROP",~pi2drop );
    FthItem("2DUP",~pi2dup );
    FthItem("2OVER",~pi2over);
    FthItem(".",~pDot);
    FthItem("U.",~pUDot);
    FthItem("H.",~pHDot);
    FthItem("CATCH",PCatch);
    FthItem("THROW",~pFThrow);
    FthItem("[",~pIMode); Immediate();
    FthItem("]",~pCMode);
    FthItem("@",~pLoad);
    FthItem("C@",~pCLoad);
    FthItem("C!",~pCStore);
    FthItem("C+!",~pCAddStore);
    FthItem("C!A",~pCStoreA);
    FthItem("W@",~pWLoad);
    FthItem("W!",~pWStore);
    FthItem("2!",~pi2Store);
    FthItem("2@",~pi2Load);
    FthItem("COUNT",~pCount);
    FthItem("!",~pStore);
    FthItem("+!",~pAddStore);
    FthItem("1+!",~pIncr);
    FthItem("0!",~pOff);
    FthItem("OFF",~pOff);
    FthItem("ON",~pOn);
    FthItem("=",~pEqual);
    FthItem("<>",~pNEqual);
    FthItem("0<",~pZLess);
    FthItem("0=",~pZEqual);
    FthItem("0<>",~pZNEqual);
    FthItem("D0=",~pDZEqual);
    FthItem("<",~pLess);
    FthItem(">",~pGreat);
    FthItem("U<",~pULess);
    FthItem("U>",~pUGreat);
    FthItem("MAX",~pMax);
    FthItem("MIN",~pMin);
    FthItem("0MAX",~pi0Max);
    FthItem(">R",~pToR);
    FthItem("R>",~pFromR);
    FthItem("RDROP",~pRDrop);
    FthItem("R@",~pRLoad);
    FthItem("2>R",~pi2ToR);
    FthItem("2R>",~pi2FromR);
    FthItem("2R@",~pi2RLoad);
    FthItem("RP@",~pRLoad);
    FthItem("RP@",~pRPGet);
    FthItem("SP@",~pSPGet);
    FthItem("RP!",~pRPSet);
    FthItem("SP!",~pSPSet);
    FthItem(",",~pCompile);
    FthItem("ALLOT",~pAllot);
    FthItem("$,",~pStrCmp);
    FthItem("<$>",~pStr);
    FthItem("EXECUTE",~pExecute);
    FthItem("SMUDGE",~pSmudge);
    FthItem("TYPE",~pType);
    FthItem("ZTYPE",~pZType);
    FthItem("CR",~pCr);
    FthItem("SPACE",~pSpace);
    FthItem("EMIT",~pEmit);
//    FthItem("PUNCH",~pPunch);
    FthItem(">IN",Pi2in);
    FthItem("PARSE-NAME",~pParseName);
    FthItem("PARSE",~pParse);
    FthItem("SHEADER",~pSHeader);
    FthItem("BUILD",~pBuild);
    FthItem("SFIND",~pSFind);
    FthItem("SEARCH-WORDLIST",~pSearchWordList);
    FthItem("UCOMPARE",~pUCompare);
    FthItem("FILL",~pFill);
    FthItem("CMOVE",~pCmove);
    FthItem("CMOVE>",~pCmove_up);
    FthItem("ZCOUNT",~pZCount);

    FthItem("KEY?",~pKeyQ);
    FthItem("KEY",~pKey);
//    sCell PKey = 
	Header("KEY");  Co2(~pDoDefer,~pKey);

//    FthItem("LASTKEY",~pLastKey);
//    FthItem("CHLASTKEY",~pChLastKey);
//    FthItem("SCANKEY",~pScanKey);
//    FthItem("KEYBCTL",~pKBctl);
//    FthItem("SCAN2UN",~pScan2Un);
//    FthItem("CURSOR",~pCursor);
//    Constant("CURSOR%",(sCell)&CursorHSize);


//    FthItem("SHIFT?",~pQShift);
//    FthItem("CTL?",~pQCtrl);
//    FthItem("ALT?",~pQAlt);


//    FthItem("TEST1",~pTest1);
//    FthItem("TEST2",~pTest2);
//    FthItem("TEST3",~pTest3);

    FthItem("IMMEDIATE",~pImmediate);
    FthItem(":",~pColon);
    FthItem(";",~pSemicolon);   Immediate();
    FthItem("IF",~pIf);         Immediate();
    FthItem("ELSE",~pElse);     Immediate();
    FthItem("THEN",~pThen);     Immediate();
    FthItem("BEGIN",~pBegin);   Immediate();
    FthItem("UNTIL",~pUntil);   Immediate();
    FthItem("AGAIN",~pAgain);   Immediate();
    FthItem("WHILE",~pWhile);   Immediate();
    FthItem("REPEAT",~pRepeat); Immediate();

    sCell PTrue = Constant("TRUE",-1);

    FthItem("EXIT",~pExit );
    Constant("STATE",(sCell) &State );
    FthItem("?STATE",~pStateQ);

    Constant("DOVAR",~pDoVar );
    Constant("DOCONST",~pDoConst );
    Constant("DODEFER",~pDoDefer );
    Constant("DP", (sCell)&here );
    Constant("LAST", (sCell)&Last );
    Constant("LASTCFA", (sCell)&LastCFA );
    VVariable("WARNING",-1);
    FthItem("HERE",~pHere);
    Constant("BL",(sCell)' ' );
    sCell PCell = Constant("CELL",sizeof(Cell) );

    FthItem("NAME>",~pFromName);
    Constant("BASE",(sCell)&numericBase);

    Header("'");   Co5(~pParseName,~pSFind,~pZEqual,~pFThrow,~pExit);

    Constant("STATE",(sCell) &State );
//    sCell PHi =
	Header("HI"); Tp("Hello!!!"); Co(~pExit);
    sCell PLastin = Constant("LASTIN", (sCell)&Lastin );
    sCell PSaveErrQ = Constant("SAVEERR?", (sCell)&SaveErrQ );

    FthItem("SAVEERR0",~pSaveErr0);
    sCell PSaveErr = Header("SAVEERR");  Co2(~pDoDefer,~pSaveErr0);
    FthItem("PRINTERR0",~pPrintErr0);

    sCell PContext = Constant("CONTEXT",(sCell) &Context );
    Constant("CURRENT",(sCell) &Current );
    Constant("IMAGE-BEGIN",(sCell)HereArea );
    Constant("FORTH-WORDLIST",(sCell) &ForthWordlist );
    Constant("CONTEXT-SIZE",ContextSize );
    sCell PSP0 = VVariable("SP0",(sCell) &StackArea[STACK_SIZE-9] );

    FthItem("R/O",~preadOnly);
    FthItem("R/W",~preadWrite);
    FthItem("W/O",~pwriteOnly);

    FthItem("OPEN-FILE",~popenFile);
    FthItem("READ-FILE",~preadFile);
    FthItem("READ-LINE",~preadLine);
    FthItem("WRITE-FILE",~pwriteFile);
    FthItem("RESIZE-FILE",~presizeFile);


    FthItem("CLOSE-FILE",~pcloseFile);

    FthItem("SYSCALL",~pSysCall);

    Constant("DEBUG-MODE",(sCell)&DebagMode);
     FthItem("DBGZTYPE",~pDbgZType);
     FthItem("DBGZTYPECR",~pDbgZTypeCr);
     FthItem("DBGH.",~pDbgHex);
     FthItem("DBG0",~pDbg0);
     FthItem("DBG1",~pDbg1);
     FthItem("DBG2",~pDbg2);
     FthItem("DBG3",~pDbg3);
     FthItem("DBG4",~pDbg4);
     FthItem("DBG5",~pDbg5);


//    FthItem("OPEN-DIR",~pOpenDir);
//    FthItem("DIRI2NAME",~pDirI2Name);
//    FthItem("DIRI2TYPE",~pDirI2Type);
//    FthItem("DIR2COUNT",~pDir2Count);
//    FthItem("CLOSE-DIR",~pCloseDir);
//    Constant("G_CLI_PATH",(sCell)&G_CLI_PATH);

//    FthItem("ZCLI",~pZCli);

    FthItem("TIB",Ptib);
    sCell PATib = Constant("ATIB",(sCell)&atib);
    sCell Pntib = Constant("#TIB",(sCell)&ntib);

    FthItem("SOURCE",~pSource);
    FthItem("SOURCE!",~pSourceSet);
    FthItem("SOURCE-ID",PSourceId);

    FthItem("ALLOCATE",~pAllocate);
    FthItem("FREE",~pFree);

    Constant("YDP", (sCell)&YDP);
    Constant("YDP0", (sCell)&YDP0);
    FthItem("YDP_FL",~pYDPFL);

//    Constant("&XPOS", (sCell)&tty_pos_x);
//    Constant("&YPOS", (sCell)&tty_pos_y);
	FthItem("SETXY",~pSetXY);
	FthItem("GETXY",~pGetXY);

//    Constant("&COLOR", (sCell)&tty_text_color);
//    Constant("&BGCOLOR", (sCell)&tty_bg_color);
    FthItem("PAGE",~(Cell)term_clear);

    sCell PErrDO1 = Header("ERROR_DO1");	Co3(PSaveErr,~pPrintErr0,~pExit);
    sCell PErrDO = Header("ERROR_DO");  Co2(~pDoDefer,PErrDO1);

    sCell PAccept = Header("ACCEPT");  Co2(~pDoDefer,~pAccept);
    sCell PQuery = Header("QUERY");
    Co4(Ptib,~pLit_,256,PAccept);
    Co5(Pntib,~pStore,Pi2in,~pOff,~pExit);


//    FthItem("QUERY",~pQuery);

    sCell PBye = Header("BYE"); Co2(~pBye,PBye);

    sCell PLitC = Header("LIT,");  Co2(~pDoDefer,~pLitCo);
    sCell PPre = Header("<PRE>");  Co2(~pDoDefer,~pNoop);
    sCell PFileRefill = Header("FREFILL");  Co2(~pDoDefer,~pNoop);
    sCell PQStack = Header("?STACK");  Co2(~pDoDefer,~pNoop);

    sCell PRefill = Header("REFILL");
	Co(PSourceId);
    If(); //  Co2(PFileRefill,~pDup);  If(); Co(PPre); Then();
	Co(PFileRefill);
    Else(); Co2(PQuery,PTrue);
    Then(); Co(~pExit);

    FthItem("SNUMBER0",~pSNumber0);

    sCell PSNumber = Header("SNUMBER");  Co2(~pDoDefer,~pSNumber0 );

    sCell PQSLiteral0 = Header("?SLITERAL0");
	Co(PSNumber);
	If(); Lit(-13); Co(~pFThrow);
	Else(); Co(~pStateQ); If(); Co(PLitC); Then();
	Then();
    Co(~pExit);

    sCell PQSLiteral = Header("?SLITERAL");
    Co2(~pDoDefer,PQSLiteral0);

    sCell PInterpret1 = Header("INTERPRET1");
    Begin();
        Co6(Pi2in,~pLoad,PLastin,~pStore,PSaveErrQ,~pOn);
        Co2(~pParseName,~pDup);
    While();  Co2(~pSFind,~pQDup);
        If();
            Co2(~pStateQ,~pEqual);
            If();   Co(~pCompile );
            Else(); Co(~pExecute );
            Then();
        Else(); Co(PQSLiteral);
        Then(); Co(PQStack);
    Repeat();
    Co2(~pi2drop,~pExit);

    sCell PInterpret = Header("INTERPRET");
    Co2(~pDoDefer,PInterpret1 );

    sCell PQuit = Header("QUIT");
    Begin();	Co(PRefill); 
    While();	Co(PInterpret);	Tp(" ok\n>");
    Repeat();	Co(~pExit);

//    sCell PWords = 
	Header("WORDS");
    Co3(PContext,~pLoad,~pLoad);
    Begin(); Co(~pDup);
    While(); Co7(~pDup,~pCount,~pType,~pSpace,PCell,~pSub,~pLoad );
    Repeat(); Co2(~pDrop,~pExit );

    ip = here;  // SYS START

	Tp("Forth\n");

    Co(~pInitStringSet);
    Co5(~pIMode,~pLit_,PInterpret,PCatch,~pQDup );
    If(); Co5(PErrDO,PSP0,~pLoad,~pSPSet,~pCr ) ;
    Then();

    Begin();
	Co4(PATib,~pLit_,(sCell)&tib[1],~pStore);
        Co5(~pIMode,~pLit_,PQuit,PCatch,PErrDO);
        Co4(PSP0,~pLoad,~pSPSet,~pCr ) ;
    Again();
}


int shell_forth_command(int argc, const char** argv) {
    u8 cc;
    forth_run=1;

    term_write("forth:", TC_WHITE);
    for (int i = 1; i < argc; i++) {
        if (i > 1)
            term_write(" ", TC_WHITE);
        term_write(argv[i], TC_WHITE);
    }

	tib[0]=~pDoConst;
	i2in[0]=~pDoVar;
	SourceId[0]=~pDoConst;
	tib[0]=~pDoConst;
	Catch[0] = ~pto_catch;
	Catch[1] = ~pfrom_catch;

	HereArea = kmalloc(HERE_SIZE1);
	StackArea = kmalloc(STACK_SIZE);
	RStackArea = kmalloc(RSTACK_SIZE);

	here = HereArea ;
	Stack = &StackArea[STACK_SIZE-8] ;
	rStack = &RStackArea[RSTACK_SIZE-8] ;

        ForthWordlist[0] = 0;
        ForthWordlist[1] = 0;
        ForthWordlist[2] = 0;

	Context[0] = ForthWordlist;
	Context[1] = 0;
	Current[0] = ForthWordlist;
	ireg = ~(sCell)MakeImag;

	add_command_to_history("S\" \" R/W OPEN-FILE H. H.");
	add_command_to_history("SYS> ls");
	add_command_to_history("HERE 222 + 22 TYPE");
	add_command_to_history("HERE 222 + 22 FID READ-FILE H. H.");
	add_command_to_history("FLOAD ForthLib/CompIF4.4th");
	add_command_to_history("FLOAD ForthLib\\CompIF4.4th");


//  dprintln("<<<<<<<<<<forth_run>>>>>>>>>>>");

  if(pNoop>0){
      term_write("positiv\n",text_color); // addresses area
	
      while (forth_run)
      {   do{
              ((proc) (~ireg) )();
              ireg = *ip++;

	if(DebagMode){dprinthex((s32)ireg); dprintchar(' ');}

          }while ( ireg<0);
          do{
              *--rStack = (sCell) ip;  ip =  (sCell *) ireg;
              ireg = *ip++;

	if(DebagMode){
	dprintln("|");
	dprinthex((sCell)ip);
        dprintchar(':');
	dprinthex((sCell)ireg);
        dprintchar('>');
	}

          }while ( ireg>0);
      }
  }
  else{
      term_write("negative\n",text_color); // addresses area
      while (forth_run)
        {   do{
                ((proc) (~ireg) )();
                ireg = *ip++;

	if(DebagMode){dprinthex((s32)ireg); dprintchar(' ');}

            }while ( ireg>0);
            do{
                *--rStack = (sCell) ip;  ip =  (sCell *) ireg;
                ireg = *ip++;
	if(DebagMode){
	dprintln("|");
	dprinthex((sCell)ip);
        dprintchar(':');
	dprinthex((sCell)ireg);
        dprintchar('>');
	}

            }while ( ireg<0);
        }
    }


}