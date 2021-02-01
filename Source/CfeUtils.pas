unit CfeUtils;

interface

uses
  Classes;

procedure Flip16(var Value);
procedure Flip32(var Value);
procedure Flip64(var Value);
procedure Flip80(var Value);

// various swap functions for converting big-endian data
function Swap16(Value: Word): Word; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function Swap32(Value: Cardinal): Cardinal;
function Swap64(Value: Int64): Int64;

function ReadSwappedWord(Stream: TStream): Word;
function ReadSwappedSmallInt(Stream: TStream): SmallInt;
function ReadSwappedCardinal(Stream: TStream): Cardinal;
function ReadSwappedInt64(Stream: TStream): Int64;

procedure WriteSwappedWord(Stream: TStream; Value: Word);
procedure WriteSwappedSmallInt(Stream: TStream; Value: SmallInt);
procedure WriteSwappedCardinal(Stream: TStream; Value: Cardinal);
procedure WriteSwappedInt64(Stream: TStream; Value: Int64);

procedure CopySwappedWord(Source: PWord; Destination: PWord; Size: Integer);

implementation

uses
  SysUtils;

{ Byte Ordering }

type
  T16Bit = record
    case Integer of
      0 :  (v: SmallInt);
      1 :  (b: array[0..1] of Byte);
  end;

  T32Bit = record
    case Integer of
      0 :  (v: LongInt);
      1 :  (b: array[0..3] of Byte);
  end;

  T64Bit = record
    case Integer of
      0 :  (v: Int64);
      1 :  (b: array[0..7] of Byte);
  end;

  T80Bit = record
    case Integer of
      0 :  (v: Extended);
      1 :  (b: array[0..9] of Byte);
  end;


procedure Flip16(var Value);
var
  t: Byte;
begin
 with T16Bit(Value) do
  begin
   t := b[0];
   b[0] := b[1];
   b[1] := t;
  end;
end;

procedure Flip32(var Value);
var
  Temp: Byte;
begin
 with T32Bit(Value) do
  begin
   Temp := b[0];
   b[0] := b[3];
   b[3] := Temp;
   Temp := b[1];
   b[1] := b[2];
   b[2] := Temp;
  end;
end;

procedure Flip64(var Value);
var
  Temp: Byte;
begin
 with T64Bit(Value) do
  begin
   Temp := b[0];
   b[0] := b[7];
   b[7] := Temp;
   Temp := b[1];
   b[1] := b[6];
   b[6] := Temp;
   Temp := b[2];
   b[2] := b[5];
   b[5] := Temp;
   Temp := b[3];
   b[3] := b[4];
   b[4] := Temp;
  end;
end;

procedure Flip80(var Value);
var
  Temp: Byte;
  T80B: T80Bit absolute Value;
begin
 with T80B do
  begin
   Temp := b[0];
   b[0] := b[9];
   b[9] := Temp;
   Temp := b[1];
   b[1] := b[8];
   b[8] := Temp;
   Temp := b[2];
   b[2] := b[7];
   b[7] := Temp;
   Temp := b[3];
   b[3] := b[6];
   b[6] := Temp;
   Temp := b[4];
   b[4] := b[5];
   b[5] := Temp;
  end;
end;

function Swap16(Value: Word): Word; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
{$IFDEF SUPPORTS_INLINE}
begin
  Result := Swap(Value);
{$ELSE}
{$IFDEF PUREPASCAL}
begin
  Result := Swap(Value);
{$ELSE}
asm
  {$IFDEF CPUx86_64}
  MOV     EAX, ECX
  {$ENDIF}
  XCHG    AL, AH
  {$ENDIF}
  {$ENDIF}
end;

function Swap32(Value: Cardinal): Cardinal;
{$IFDEF PUREPASCAL}
type
  TTwoWords = array [0..1] of Word;
begin
  TTwoWords(Result)[1] := Swap(TTwoWords(Value)[0]);
  TTwoWords(Result)[0] := Swap(TTwoWords(Value)[1]);
{$ELSE}
asm
  {$IFDEF CPUx86_64}
  MOV     EAX, ECX
  {$ENDIF}
  BSWAP   EAX
  {$ENDIF}
end;

function Swap64(Value: Int64): Int64;
type
  TFourWords = array [0..3] of Word;
begin
  TFourWords(Result)[3] := Swap(TFourWords(Value)[0]);
  TFourWords(Result)[2] := Swap(TFourWords(Value)[1]);
  TFourWords(Result)[1] := Swap(TFourWords(Value)[2]);
  TFourWords(Result)[0] := Swap(TFourWords(Value)[3]);
end;

function ReadSwappedWord(Stream: TStream): Word;
begin
{$IFDEF ValidateEveryReadOperation}
  if Stream.Read(Result, SizeOf(Word)) <> SizeOf(Word) then
    raise EPascalTypeStremReadError.Create(RCStrStreamReadError);
{$ELSE}
  Stream.Read(Result, SizeOf(Word));
{$ENDIF}
  Result := Swap16(Result);
end;

function ReadSwappedSmallInt(Stream: TStream): SmallInt;
begin
{$IFDEF ValidateEveryReadOperation}
  if Stream.Read(Result, SizeOf(SmallInt)) <> SizeOf(SmallInt) then
    raise EPascalTypeStremReadError.Create(RCStrStreamReadError);
{$ELSE}
  Stream.Read(Result, SizeOf(SmallInt));
{$ENDIF}
  Result := Swap16(Result);
end;

function ReadSwappedCardinal(Stream: TStream): Cardinal;
begin
{$IFDEF ValidateEveryReadOperation}
  Assert(SizeOf(Cardinal) = 4);
  if Stream.Read(Result, SizeOf(Cardinal)) <> SizeOf(Cardinal) then
    raise EPascalTypeStremReadError.Create(RCStrStreamReadError);
{$ELSE}
  Stream.Read(Result, SizeOf(Cardinal));
{$ENDIF}
  Result := Swap32(Result);
end;

function ReadSwappedInt64(Stream: TStream): Int64;
begin
{$IFDEF ValidateEveryReadOperation}
  if Stream.Read(Result, SizeOf(Int64)) <> SizeOf(Int64) then
    raise EPascalTypeStremReadError.Create(RCStrStreamReadError);
{$ELSE}
  Stream.Read(Result, SizeOf(Int64));
{$ENDIF}
  Result := Swap64(Result);
end;

procedure WriteSwappedWord(Stream: TStream; Value: Word);
begin
  Value := Swap16(Value);
  Stream.Write(Value, SizeOf(Word));
end;

procedure WriteSwappedSmallInt(Stream: TStream; Value: SmallInt);
begin
  Value := Swap16(Value);
  Stream.Write(Value, SizeOf(SmallInt));
end;

procedure WriteSwappedCardinal(Stream: TStream; Value: Cardinal);
begin
  Value := Swap32(Value);
  Stream.Write(Value, SizeOf(Cardinal));
end;

procedure WriteSwappedInt64(Stream: TStream; Value: Int64);
begin
  Value := Swap64(Value);
  Stream.Write(Value, SizeOf(Int64));
end;

procedure CopySwappedWord(Source: PWord; Destination: PWord; Size: Integer);
var
  Cnt: Integer;
begin
  for Cnt := 0 to Size - 1 do
  begin
    Destination^ := Swap16(Source^);
    Inc(Source);
    Inc(Destination);
  end;
end;

end.
