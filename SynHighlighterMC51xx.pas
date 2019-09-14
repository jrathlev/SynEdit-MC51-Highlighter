{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

Basic code template generated with SynGen.
The original code is: SynHighlighterMC51.pas, released 2005-06-23.
Description: Syntax Parser/Highlighter
The initial author of this file is J. Rathlev.
Copyright (c) 2009, all rights reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: $

You may retrieve the latest version at the SynEdit home page,
located at https://github.com/SynEdit/SynEdit

-------------------------------------------------------------------------------
SynEdit-Highlighter for 51-family microcontrollers
Includes highlighting for all members of the 51-family
Special function registers (SFR) are loaded from file
You need a file with SFR declarations (<name>.mcu) for each member
Loading of this file is done by "InitHighlighter(Filename)"  
J. Rathlev, Uni-Kiel, June 2005
last modified: September 2019  (kontakt(a)rathlev-home.de)
-------------------------------------------------------------------------------}

unit SynHighlighterMC51xx;     // Unicode version

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  QGraphics,
  QSynEditTypes,
  QSynEditHighlighter,
{$ELSE}
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
{$ENDIF}
  SysUtils,
  Classes;

type
  TtkTokenKind = (
    tkCodeAddress,
    tkComment,
    tkConstant,
    tkControl,
    tkDirective,
    tkIdentifier,
    tkInstruction,
    tkMetaInst,
    tkNull,
    tkOperator,
    tkSpace,
    tkSpecBitReg,
    tkSpecDataReg,
    tkSpecSymbol,
    tkString,
    tkUnknown);

  TRangeState = (rsUnKnown, rsComment, rsConstant, rsSString, rsDString);

  TProcTableProc = procedure of object;

const
  MaxKey = 255;

type
  TSynMc51xxSyn = class(TSynCustomHighlighter)
  private
    fRange: TRangeState;
    fTokenID: TtkTokenKind;
    fProcTable: array[#0..#255] of TProcTableProc;
    fIdentFuncTable: array[0 .. MaxKey] of TStringList;
    fCodeAddressAttri: TSynHighlighterAttributes;
    fCommentAttri: TSynHighlighterAttributes;
    fConstantAttri: TSynHighlighterAttributes;
    fControlAttri: TSynHighlighterAttributes;
    fDirectiveAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fInstructionAttri: TSynHighlighterAttributes;
    fMetaInstAttri: TSynHighlighterAttributes;
    fOperatorAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fSpecBitRegAttri: TSynHighlighterAttributes;
    fSpecDataRegAttri: TSynHighlighterAttributes;
    fSpecSymbolAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    function KeyHash(ToHash: PWideChar): Integer;
    function KeyComp(const aKey: string): Boolean;
    procedure IdentProc;
    procedure UnknownProc;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    function GetKind (HashKey : integer) : TtkTokenKind;
    procedure MakeMethodTables;
    procedure NullProc;
    procedure SpaceProc;
    procedure CRProc;
    procedure LFProc;
    procedure CommentOpenProc;
    procedure CommentProc;
    procedure ConstantOpenProc;
    procedure ConstantProc;
    procedure SStringOpenProc;
    procedure SStringProc;
    procedure DStringOpenProc;
    procedure DStringProc;
    procedure AddIdent (const AKey : string; AToken : TtkTokenKind);
  protected
    function GetSampleSource: string; override;
    function IsFilterStored: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IsIdentChar(AChar: WideChar): Boolean; override;
    function IsWhiteChar(AChar: WideChar): Boolean; override;
    function IsWordBreakChar(AChar: WideChar): Boolean; override;
    {$IFNDEF SYN_CPPB_1} class {$ENDIF}
    function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: UnicodeString; override;
    function GetRange: Pointer; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes; override;
    function GetEol: Boolean; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    procedure Next; override;
    function InitHighlighter (const McuFile : string) : boolean;
  published
    property CodeAddressAttri: TSynHighlighterAttributes read fCodeAddressAttri write fCodeAddressAttri;
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri write fCommentAttri;
    property ConstantAttri: TSynHighlighterAttributes read fConstantAttri write fConstantAttri;
    property ControlAttri: TSynHighlighterAttributes read fControlAttri write fControlAttri;
    property DirectiveAttri: TSynHighlighterAttributes read fDirectiveAttri write fDirectiveAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri write fIdentifierAttri;
    property InstructionAttri: TSynHighlighterAttributes read fInstructionAttri write fInstructionAttri;
    property MetaInstAttri: TSynHighlighterAttributes read fMetaInstAttri write fMetaInstAttri;
    property OperatorAttri: TSynHighlighterAttributes read fOperatorAttri write fOperatorAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri write fSpaceAttri;
    property SpecBitRegAttri: TSynHighlighterAttributes read fSpecBitRegAttri write fSpecBitRegAttri;
    property SpecDataRegAttri: TSynHighlighterAttributes read fSpecDataRegAttri write fSpecDataRegAttri;
    property SpecSymbolAttri: TSynHighlighterAttributes read fSpecSymbolAttri write fSpecSymbolAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri write fStringAttri;
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditStrConst;
{$ELSE}
  SynEditStrConst;
{$ENDIF}

{$IFDEF SYN_COMPILER_3_UP}
resourcestring
{$ELSE}
const
{$ENDIF}
  SYNS_FilterAsm51 = 'Mc51 Assembler (*.a51)|*.a51';
  SYNS_FriendlyLangMC51 = 'Mc51 Assembler';
  SYNS_LangAsm51 = 'Asm 51';

  SYNS_AttrCodeAddress = 'CodeAddress';
  SYNS_AttrConstant = 'Constant';
  SYNS_AttrControl = 'Control';
  SYNS_AttrInstruction = 'Instruction';
  SYNS_AttrMetaInst = 'MetaInst';
  SYNS_AttrSpecBitReg = 'SpecBitReg';
  SYNS_AttrSpecDataReg = 'SpecDataReg';
  SYNS_AttrSpecSymbol = 'SpecSymbol';

  SYNS_FriendlyAttrCodeAddress = 'Code Address';
  SYNS_FriendlyAttrConstant = 'Constant';
  SYNS_FriendlyAttrControl = 'Assembler Control';
  SYNS_FriendlyAttrInstruction = 'Assembler Instruction';
  SYNS_FriendlyAttrMetaInst = 'Meta Instruction';
  SYNS_FriendlyAttrSpecBitReg = 'Special Bit Register';
  SYNS_FriendlyAttrSpecDataReg = 'Special Data Register';
  SYNS_FriendlyAttrSpecSymbol = 'Special Symbol';

// Default instructions for 51 microcontroller family
// The SFRs must be loaded from a MCU file (see InitHighlighter), if not found the
// default values are set
const
  IdChars : set of AnsiChar = ['_','?','$','0'..'9','a'..'z','A'..'Z','+','-','*','/','=','<','>'];
  IdStart : set of AnsiChar =  // start of key including operators
     ['_','?','$','a'..'z','A'..'Z','+','-','*','/','=','<','>'];

  WbChars : set of AnsiChar = [#0..#32,'.',',',';',':','"','''','´','`','°','^','!','&',
      '§','%','~','[',']','(',')','{','}','<','>','-','=','+','*','/','\','|'];

  Instructions : string = 'ACALL ADD ADDC AJMP ANL CALL CJNE CLR CPL '+
    'DA DEC DIV DJNZ INC JB JBC JC JMP JNB JNC JNZ JZ LCALL LJMP MOV '+
    'MOVC MOVX MUL NOP ORL POP PUSH RET RETI RL RLC RR RRC SETB SJMP '+
    'SUBB SWAP XCH XCHD XRL';

  SpecSymbols : string = '$ A AB AR0 AR1 AR2 AR3 AR4 AR5 AR6 AR7 '+
    'C DPTR PC R0 R1 R2 R3 R4 R5 R6 R7';

  Directives : string = 'AT BIT BSEG CODE CSEG DATA DB DBIT DS DSEG DW END '+
      'EQU IDATA ISEG NAME ORG SET USING XDATA XSEG';

  Operators : string = 'AND EQ GE GT HIGH LE LOW LT MOD NE NOT OR SHL '+
    'SHR XOR + - * / = <> < > <= >=';

  Controls : string = '$COND $CONDONLY $DA $DATE $DB $DEBUG $EJ $EJECT $ERROR '+
    '$GE $GEN $GENONLY $GO $IC $INCLUDE $LI $LIST $MACRO $MO $MOD51 $MR '+
    '$NOBUILTIN $NOCOND $NODB $NODEBUG $NOGE $NOGEN $NOLI $NOLIST $NOMACRO '+
    '$NOMO $NOMOD51 $NOMR $NOPAGING $NOPI $NOSB $NOSYMBOLS $NOTABS $NOXR '+
    '$NOXREF $PAGELENGTH $PAGEWIDTH $PAGING $PHILIPS $PI $PL $PW $RESTORE '+
    '$RS $SA $SAVE $SB $SYMBOLS $TITLE $TT $WARNING $XR $XREF';

  MetaInsts : string = 'ELSE ELSEIF ELSEIFB ELSEIFDEF ELSEIFN ELSEIFNB '+
    'ELSEIFNDEF ENDIF ENDM EXITM IF IFB IFDEF IFN IFNB IFNDEF LOCAL '+
    'MACRO REPT';

  // Default values for SFRs (standard 8051)
  SpecDataRegs : string = 'P0 SP DPL DPH PCON TCON TMOD TL0 TL1 TH0 TH1 P1 '+
    'SCON SBUF P2 IE P3 IP PSW ACC B';

  SpecBitRegs : string = 'IT0 IE0 IT1 IE1 TR0 TF0 TR1 TF1 RI TI RB8 TB8 '+
    'REN SM2 SM1 SM0 EX0 ET0 EX1 ET1 ES EA RXD TXD INT0 INT1 T0 T1 WR RD '+
    'PX0 PT0 PX1 PT1 PS P OV RS0 RS1 F0 AC CY';
    
  CodeAddresses : string = 'RESET EXTI0 TIMER0 EXTI1 TIMER1 SINT';

var
  Identifiers: array[#0..#255] of ByteBool;
  mHashTable : array[#0..#255] of Integer;

procedure MakeIdentTable;
var
  I, J: Char;
begin
  for I:=#0 to #255 do begin
    Identifiers[I]:=CharInSet(I,IdChars);
    J:=UpCase(I);
    if CharInSet(I,IdStart) then mHashTable[I]:=Ord(J) - 32
    else mHashTable[I]:=0;
    end;
  end;

function TSynMc51xxSyn.KeyHash(ToHash: PWideChar): Integer;
begin
  Result:=0;
  while IsIdentChar(ToHash^) do begin
    inc(Result, mHashTable[ToHash^]);
    inc(ToHash);
    end;
  fStringLen:=ToHash - fToIdent;
  Result:=Result and $FF;
  end;

function TSynMc51xxSyn.KeyComp(const aKey: String): Boolean;
var
  I: Integer;
  Temp: PWideChar;
begin
  Temp:=fToIdent;
  if Length(aKey) = fStringLen then
  begin
    Result:=True;
    for i:=1 to fStringLen do
    begin
      if mHashTable[Temp^] <> mHashTable[aKey[i]] then
      begin
        Result:=False;
        break;
      end;
      inc(Temp);
      end;
    end
  else Result:=False;
  end;

function TSynMc51xxSyn.GetKind (HashKey : integer) : TtkTokenKind;
var
  i : integer;
begin
  with fIdentFuncTable[HashKey] do if Count>0 then begin
    for i:=0 to Count-1 do if KeyComp(Strings[i]) then begin
      Result:=TtkTokenKind(Objects[i]);
      Exit;
      end
    end;
  Result:=tkIdentifier;
  end;

function TSynMc51xxSyn.IdentKind(MayBe: PWideChar) : TtkTokenKind;
var
  HashKey: Integer;
begin
  fToIdent:=MayBe;
  HashKey:=KeyHash(MayBe);
  if HashKey <= MaxKey then Result:=GetKind(HashKey)
  else Result:=tkIdentifier;
  end;

procedure TSynMc51xxSyn.MakeMethodTables;
var
  I: Char;
begin
  for I:=#0 to #255 do begin
    case I of
      #0: fProcTable[I]:=NullProc;
      #10: fProcTable[I]:=LFProc;
      #13: fProcTable[I]:=CRProc;
      #59: fProcTable[I]:=CommentOpenProc;    // ;..
      #35: fProcTable[I]:=ConstantOpenProc;   // #..
      #39: fProcTable[I]:=SStringOpenProc;
      #34: fProcTable[I]:=DStringOpenProc;
      #1..#9,
      #11,
      #12,
      #14..#32 : fProcTable[I]:=SpaceProc;
    else
      if CharInSet(I,IdStart)  then fProcTable[I]:=IdentProc
      else fProcTable[I]:=UnknownProc;
      end;
    end;
  end;

procedure TSynMc51xxSyn.SpaceProc;
begin
  fTokenID:=tkSpace;
  repeat
    inc(Run);
    until not CharInSet(fLine[Run],[#1..#32]);
  end;

procedure TSynMc51xxSyn.NullProc;
begin
  fTokenID:=tkNull;
end;

procedure TSynMc51xxSyn.CRProc;
begin
  fTokenID:=tkSpace;
  inc(Run);
  if fLine[Run] = #10 then inc(Run);
  end;

procedure TSynMc51xxSyn.LFProc;
begin
  fTokenID:=tkSpace;
  inc(Run);
  end;

procedure TSynMc51xxSyn.CommentOpenProc;
begin
  Inc(Run);
  fRange:=rsComment;
  CommentProc;
  fTokenID:=tkComment;
  end;

procedure TSynMc51xxSyn.CommentProc;
begin
  fTokenID:=tkComment;
  repeat
    if not CharInSet(fLine[Run],[#0, #10, #13]) then Inc(Run);
    until CharInSet(fLine[Run],[#0, #10, #13]);
  end;

procedure TSynMc51xxSyn.ConstantOpenProc;
begin
  Inc(Run);
  fRange:=rsConstant;
  ConstantProc;
  fTokenID:=tkConstant;
  end;

procedure TSynMc51xxSyn.ConstantProc;
begin
  fTokenID:=tkConstant;
  repeat
    if CharInSet(fLine[Run],[#32,#9,';',',']) then begin
      fRange:=rsUnKnown;
      Break;
      end;
    if not CharInSet(fLine[Run],[#0, #10, #13]) then Inc(Run);
    until CharInSet(fLine[Run],[#0, #10, #13]);
  end;

procedure TSynMc51xxSyn.SStringOpenProc;
begin
  Inc(Run);
  fRange:=rsSString;
  SStringProc;
  fTokenID:=tkString;
  end;

procedure TSynMc51xxSyn.SStringProc;
begin
  fTokenID:=tkString;
  repeat
    if (fLine[Run] = #39) then begin
      Inc(Run, 1);
      fRange:=rsUnKnown;
      Break;
      end;
    if not CharInSet(fLine[Run],[#0, #10, #13]) then Inc(Run);
    until CharInSet(fLine[Run],[#0, #10, #13]);
  end;

procedure TSynMc51xxSyn.DStringOpenProc;
begin
  Inc(Run);
  fRange:=rsDString;
  DStringProc;
  fTokenID:=tkString;
end;

procedure TSynMc51xxSyn.DStringProc;
begin
  fTokenID:=tkString;
  repeat
    if (fLine[Run] = #34) then begin
      Inc(Run, 1);
      fRange:=rsUnKnown;
      Break;
    end;
    if not CharInSet(fLine[Run],[#0, #10, #13]) then Inc(Run);
    until CharInSet(fLine[Run],[#0, #10, #13]);
  end;

constructor TSynMc51xxSyn.Create(AOwner: TComponent);
var
  i : integer;
begin
  inherited Create(AOwner);
  fCodeAddressAttri:=TSynHighLighterAttributes.Create(SYNS_AttrCodeAddress,SYNS_FriendlyAttrCodeAddress);
  fCodeAddressAttri.Style:=[fsBold];
  fCodeAddressAttri.Foreground:=clGreen;
  AddAttribute(fCodeAddressAttri);

  fCommentAttri:=TSynHighLighterAttributes.Create(SYNS_AttrComment,SYNS_FriendlyAttrComment);
  fCommentAttri.Style:=[fsItalic];
  fCommentAttri.Foreground:=clGreen;
  AddAttribute(fCommentAttri);

  fConstantAttri:=TSynHighLighterAttributes.Create(SYNS_AttrConstant,SYNS_FriendlyAttrConstant);
  fConstantAttri.Style:=[fsItalic];
  fConstantAttri.Foreground:=clBlue;
  fConstantAttri.Background:=clYellow;
  AddAttribute(fConstantAttri);

  fControlAttri:=TSynHighLighterAttributes.Create(SYNS_AttrControl,SYNS_FriendlyAttrControl);
  fControlAttri.Style:=[fsItalic];
  fControlAttri.Foreground:=clRed;
  AddAttribute(fControlAttri);

  fDirectiveAttri:=TSynHighLighterAttributes.Create(SYNS_AttrDirective,SYNS_FriendlyAttrDirective);
  fDirectiveAttri.Foreground:=clBlue;
  AddAttribute(fDirectiveAttri);

  fIdentifierAttri:=TSynHighLighterAttributes.Create(SYNS_AttrIdentifier,SYNS_FriendlyAttrIdentifier);
  AddAttribute(fIdentifierAttri);

  fInstructionAttri:=TSynHighLighterAttributes.Create(SYNS_AttrInstruction,SYNS_FriendlyAttrInstruction);
  fInstructionAttri.Style:=[fsBold];
  AddAttribute(fInstructionAttri);

  fMetaInstAttri:=TSynHighLighterAttributes.Create(SYNS_AttrMetaInst,SYNS_FriendlyAttrMetaInst);
  fMetaInstAttri.Style:=[fsItalic];
  fMetaInstAttri.Foreground:=clBlue;
  fMetaInstAttri.Background:=clSilver;
  AddAttribute(fMetaInstAttri);

  fOperatorAttri:=TSynHighLighterAttributes.Create(SYNS_AttrOperator,SYNS_FriendlyAttrOperator);
  fOperatorAttri.Foreground:=clRed;
  AddAttribute(fOperatorAttri);

  fSpaceAttri:=TSynHighLighterAttributes.Create(SYNS_AttrSpace,SYNS_FriendlyAttrSpace);
  AddAttribute(fSpaceAttri);

  fSpecBitRegAttri:=TSynHighLighterAttributes.Create(SYNS_AttrSpecBitReg,SYNS_FriendlyAttrSpecBitReg);
  fSpecBitRegAttri.Style:=[fsBold];
  fSpecBitRegAttri.Foreground:=clRed;
  AddAttribute(fSpecBitRegAttri);

  fSpecDataRegAttri:=TSynHighLighterAttributes.Create(SYNS_AttrSpecDataReg,SYNS_FriendlyAttrSpecDataReg);
  fSpecDataRegAttri.Style:=[fsBold];
  fSpecDataRegAttri.Foreground:=clBlue;
  AddAttribute(fSpecDataRegAttri);

  fSpecSymbolAttri:=TSynHighLighterAttributes.Create(SYNS_AttrSpecSymbol,SYNS_FriendlyAttrSpecSymbol);
  fSpecSymbolAttri.Style:=[fsBold];
  fSpecSymbolAttri.Foreground:=clMaroon;
  AddAttribute(fSpecSymbolAttri);

  fStringAttri:=TSynHighLighterAttributes.Create(SYNS_AttrString,SYNS_FriendlyAttrString);
  fStringAttri.Foreground:=clRed;
  AddAttribute(fStringAttri);

  SetAttributesOnChange(DefHighlightChange);
  for i:=0 to MaxKey do fIdentFuncTable[i]:=TStringList.Create;
  InitHighlighter('');
  MakeMethodTables;
  fDefaultFilter:=SYNS_FilterAsm51;
  fRange:=rsUnknown;
  end;

destructor TSynMc51xxSyn.Destroy;
var
  i : integer;
begin
  for i:=0 to MaxKey do fIdentFuncTable[i].Free;
  inherited Destroy;
  end;

procedure TSynMc51xxSyn.AddIdent (const AKey : string; AToken : TtkTokenKind);
var
  n : integer;
begin
  n:=KeyHash(PWideChar(AKey));
  if (n>=0) and (n<=MaxKey) then with fIdentFuncTable[n] do AddObject(AKey,pointer(AToken));
  end;

function TSynMc51xxSyn.InitHighlighter (const McuFile : string) : boolean;
var
  i   : integer;
  s,t : string;
  mt  : TextFile;

  function ReadNextStr (var s : String) : string;
  var
    i,j : integer;
  begin
    if length(s)>0 then begin
      i:=pos (' ',s); j:=pos(#9,s); // Space or Tab
      if (i=0) or ((j>0) and (j<i)) then i:=j;
      if i=0 then i:=succ(length(s));
      Result:=copy(s,1,pred(i));
      delete(s,1,i);
      end
    else Result:='';
    end;

  procedure AddKeys (Keys : string;  AToken : TtkTokenKind);
  begin
    repeat
      AddIdent(ReadNextStr(Keys),AToken)
      until length(Keys)=0;
    end;

begin
  for i:=0 to MaxKey do fIdentFuncTable[i].Clear;
  AddKeys(Instructions,tkInstruction);
  AddKeys(SpecSymbols,tkSpecSymbol);
  AddKeys(Directives,tkDirective);
  AddKeys(Operators,tkOperator);
  AddKeys(Controls,tkControl);
  AddKeys(MetaInsts,tkMetaInst);
  if FileExists(McuFile) then begin
    AssignFile(mt,McuFile); reset(mt);
    while not Eof(mt) do begin
      readln (mt,s);
      s:=Trim (s);
      if (length(s)>0) and (s[1]<>';') then begin
        t:=ReadNextStr(s); s:=TrimLeft(s);
        s:=ReadNextStr(s);
        if UpperCase(s)='DATA' then AddIdent(t,tkSpecDataReg)
        else if UpperCase(s)='BIT' then AddIdent(t,tkSpecBitReg)
        else if UpperCase(s)='CODE' then AddIdent(t,tkCodeAddress);
        end;
      end;
    CloseFile(mt);
    Result:=true;
    end
  else begin // use standard SFRs
    AddKeys(SpecDataRegs,tkSpecDataReg);
    AddKeys(SpecBitRegs,tkSpecBitReg);
    AddKeys(CodeAddresses,tkCodeAddress);
    Result:=false;
    end;
  end;

procedure TSynMc51xxSyn.IdentProc;
begin
  fTokenID:=IdentKind((fLine + Run));
  inc(Run, fStringLen);
  while Identifiers[fLine[Run]] do Inc(Run);
end;

procedure TSynMc51xxSyn.UnknownProc;
begin
  inc(Run);
  fTokenID:=tkUnknown;
  end;

procedure TSynMc51xxSyn.Next;
var
  w : WideChar;
begin
  fTokenPos:=Run;
  w:=fLine[Run];
  if w<=#255 then fProcTable[w]
  else UnknownProc;

  // ensure that one call of Next is enough to reach next token
  if (fOldRun = Run) and not GetEol then Next;

  inherited;
  end;

function TSynMc51xxSyn.GetDefaultAttribute(Index: integer): TSynHighLighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT    : Result:=fCommentAttri;
    SYN_ATTR_IDENTIFIER : Result:=fIdentifierAttri;
    SYN_ATTR_STRING     : Result:=fStringAttri;
    SYN_ATTR_WHITESPACE : Result:=fSpaceAttri;
  else Result:=nil;
    end;
  end;

function TSynMc51xxSyn.GetEol: Boolean;
begin
  Result:=fTokenID = tkNull;
  end;

function TSynMc51xxSyn.GetTokenAttribute: TSynHighLighterAttributes;
begin
  case fTokenId of
    tkCodeAddress: Result:=fCodeAddressAttri;
    tkComment: Result:=fCommentAttri;
    tkConstant: Result:=fConstantAttri;
    tkControl: Result:=fControlAttri;
    tkDirective: Result:=fDirectiveAttri;
    tkIdentifier: Result:=fIdentifierAttri;
    tkInstruction: Result:=fInstructionAttri;
    tkMetaInst: Result:=fMetaInstAttri;
    tkOperator: Result:=fOperatorAttri;
    tkSpace: Result:=fSpaceAttri;
    tkSpecBitReg: Result:=fSpecBitRegAttri;
    tkSpecDataReg: Result:=fSpecDataRegAttri;
    tkSpecSymbol: Result:=fSpecSymbolAttri;
    tkString: Result:=fStringAttri;
    tkUnknown: Result:=fIdentifierAttri;
  else Result:=nil;
    end;
  end;

function TSynMc51xxSyn.GetTokenKind: integer;
begin
  Result:=Ord(fTokenId);
  end;

function TSynMc51xxSyn.IsIdentChar(AChar: WideChar): Boolean;
begin
  Result:=CharInSet(AChar,IdChars);
  end;

function TSynMc51xxSyn.IsWhiteChar(AChar: WideChar): Boolean;
begin
  if CharInSet(AChar,[#0..#32]) then Result := True
  else Result := not (IsIdentChar(AChar) or IsWordBreakChar(AChar))
  end;

function TSynMc51xxSyn.IsWordBreakChar(AChar: WideChar): Boolean;
begin
  Result:=CharInSet(AChar,WbChars);
  end;

function TSynMc51xxSyn.GetSampleSource: string;
begin
  Result:=';************************************************************************'#13#10 +
            '; Sample program'#13#10 +
            #13#10 +
            '        CODEMEM EQU     000H'#13#10 +
            '        CSEG    AT      CODEMEM'#13#10 +
            '        ORG     CODEMEM+30H'#13#10 +
            #13#10 +
            '$INCLUDE(Includes\Sqrt.a51)'#13#10 +
            #13#10 +
            'MOVCD2: MOV     R7,#2'#13#10 +
            'MOVCD:  CLR     A'#13#10 +
            '        MOVC    A,@A+DPTR'#13#10 +
            '        INC     DPTR'#13#10 +
            '        MOV     @R0,A'#13#10 +
            '        INC     R0'#13#10 +
            '        DJNZ    R7,MOVCD'#13#10 +
            '        RET';
  end;

function TSynMc51xxSyn.IsFilterStored: Boolean;
begin
  Result:=fDefaultFilter <> SYNS_FilterAsm51;
  end;

{$IFNDEF SYN_CPPB_1} class {$ENDIF}
function TSynMc51xxSyn.GetLanguageName: string;
begin
  Result:=SYNS_LangAsm51;
  end;

procedure TSynMc51xxSyn.ResetRange;
begin
  fRange:=rsUnknown;
  end;

procedure TSynMc51xxSyn.SetRange(Value: Pointer);
begin
  fRange:=TRangeState(Value);
  end;

function TSynMc51xxSyn.GetRange: Pointer;
begin
  Result:=Pointer(fRange);
  end;

class function TSynMc51xxSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangMC51;
  end;

initialization
  MakeIdentTable;
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynMc51xxSyn);
{$ENDIF}
end.
