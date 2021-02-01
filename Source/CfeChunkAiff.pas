unit CfeChunkAiff;

interface

uses
  Classes, SysUtils, CfeChunkCommon;

const
  AIFCVersion1 = $A2805140;

type
  TAiffCompressionType = (ctNotAvailable, ctNone, ctFL32, ctFL64, ctACE2,
    ctACE8, ctMACE3, ctMACE6, ctALAW, ctULAW, ctG722, ctG726, ctG728, ctGSM,
    ctUnknown);

  TAiffDefinedChunk = class(TDefinedChunk)
  public
    constructor Create; override;
  end;

  TAiffFixedDefinedChunk = class(TFixedDefinedChunk)
  public
    constructor Create; override;
  end;

  TAiffUnknownChunk = class(TUnknownChunk)
  public
    constructor Create; override;
  end;

  TAiffTextChunk = class(TCustomTextChunk)
  public
    constructor Create; override;
  end;

  TAiffCommonRecord = packed record
    Channels: SmallInt;
    SampleFrames: Cardinal;
    SampleSize: SmallInt;
    SampleRate: Extended;
  end;

  TAiffCommonChunk = class(TAiffDefinedChunk) // 'COMM'
  private
    FCompressionType: TAiffCompressionType;
    FCompressionName: string;
    FForceReadCompression: Boolean;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure CalculateChunkSize; virtual;
  public
    AIFFCommonRecord: TAiffCommonRecord;
    constructor Create; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    class function GetClassChunkName: TChunkName; override;

    property Channels: SmallInt read AIFFCommonRecord.Channels;
    property SampleFrames: Cardinal read AIFFCommonRecord.SampleFrames;
    property SampleSize: SmallInt read AIFFCommonRecord.SampleSize;
    property SampleRate: Extended read AIFFCommonRecord.SampleRate;
    property Compression: TAiffCompressionType read FCompressionType write FCompressionType;
    property ForceReadCompression: Boolean read FForceReadCompression write FForceReadCompression;
  end;

  TAiffFormRecord = packed record
    FormType: TChunkName; // type of file
  end;

  TAiffFormChunk = class(TAiffFixedDefinedChunk)
  private
    function GetFormType: string;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    AIFFFormRecord: TAiffFormRecord;
    constructor Create; override;
    class function GetClassChunkSize: Cardinal; override;
    class function GetClassChunkName: TChunkName; override;

    property FormType: string read GetFormType;
  end;

  TAiffFormatVersionRecord = packed record
    TimeStamp: Cardinal; // date of format version
  end;

  TAiffFormatVersionChunk = class(TAiffFixedDefinedChunk)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    AIFFFormatVersionRecord: TAiffFormatVersionRecord;
    constructor Create; override;
    class function GetClassChunkSize: Cardinal; override;
    class function GetClassChunkName: TChunkName; override;

    property TimeStamp: Cardinal read AIFFFormatVersionRecord.TimeStamp;
  end;

  TAiffSoundDataRecord = packed record
    Offset: Cardinal;
    BlockSize: Cardinal;
  end;

  TAiffSoundDataChunk = class(TAiffDefinedChunk) // 'SSND'
  private
    FMemoryStream: TMemoryStream;
    FSoundDataRecord: TAiffSoundDataRecord;
    procedure CalculateChunkSize;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    class function GetClassChunkName: TChunkName; override;

    property Offset: Cardinal read FSoundDataRecord.Offset;
    property BlockSize: Cardinal read FSoundDataRecord.BlockSize;
  end;

  TMarkerID = SmallInt;

  TAiffMarkerRecord = packed record
    MarkerID: TMarkerID;
    Position: Cardinal;
  end;

  TAiffMarkerItem = class(TCollectionItem)
  private
    FMarkerName: string;
  protected
    function GetDisplayName: string; override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure SetDisplayName(const Value: string); override;
  public
    MarkerRecord: TAiffMarkerRecord;
    function GetSize: Cardinal;
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);

    property DisplayName;
    property MarkerID: TMarkerID read MarkerRecord.MarkerID;
    property Position: Cardinal read MarkerRecord.Position write MarkerRecord.Position;
    property MarkerName: string read FMarkerName write FMarkerName;
  end;

  TAiffMarkerChunk = class(TAiffDefinedChunk) // 'MARK'
  private
    FMarkers: TOwnedCollection;
    procedure CalculateChunkSize;
    function GetMarkerCount: Byte;
    function GetMarker(Index: Integer): TAiffMarkerItem;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    class function GetClassChunkName: TChunkName; override;

    property MarkerCount: Byte read GetMarkerCount;
    property Marker[Index: Integer]: TAiffMarkerItem read GetMarker;
  end;

  TAiffCommentRecord = packed record
    TimeStamp: Cardinal;
    MarkerID: TMarkerID;
  end;

  TAiffCommentItem = class(TCollectionItem)
  private
    FComment: string;
  protected
    function GetDisplayName: string; override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure SetDisplayName(const Value: string); override;
  public
    CommentRecord: TAiffCommentRecord;
    function GetSize: Cardinal;
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    property DisplayName;

    property MarkerID: TMarkerID read CommentRecord.MarkerID;
    property TimeStamp: Cardinal read CommentRecord.TimeStamp write CommentRecord.TimeStamp;
    property Comment: string read FComment write FComment;
  end;

  TAiffCommentChunk = class(TAiffDefinedChunk) // 'COMT'
  private
    FComments: TOwnedCollection;
    procedure CalculateChunkSize;
    function GetCommentCount: Byte;
    function GetComment(Index: Integer): TAiffCommentItem;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    class function GetClassChunkName: TChunkName; override;

    property CommentCount: Byte read GetCommentCount;
    property Comment[Index: Integer]: TAiffCommentItem read GetComment;
  end;

  TAiffInstrumentRecord = packed record // 'INST'
    BaseNote: Byte;
    Detune: ShortInt;
    LowNote: Byte;
    HighNote: Byte;
    LowVelocity: Byte;
    HighVelocity: Byte;
    Gain: ShortInt;
    SustainLoop: TMarkerID;
    ReleaseLoop: TMarkerID;
  end;

  TAiffInstrumentChunk = class(TAiffFixedDefinedChunk)
  private
    FInstrumentRecord: TAiffInstrumentRecord;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    class function GetClassChunkSize: Cardinal; override;
    class function GetClassChunkName: TChunkName; override;

    property BaseNote: Byte read FInstrumentRecord.BaseNote;
    property Detune: ShortInt read FInstrumentRecord.Detune;
    property LowNote: Byte read FInstrumentRecord.LowNote;
    property HighNote: Byte read FInstrumentRecord.HighNote;
    property LowVelocity: Byte read FInstrumentRecord.LowVelocity;
    property HighVelocity: Byte read FInstrumentRecord.HighVelocity;
    property Gain: ShortInt read FInstrumentRecord.Gain;
    property SustainLoop: TMarkerID read FInstrumentRecord.SustainLoop;
    property ReleaseLoop: TMarkerID read FInstrumentRecord.ReleaseLoop;
  end;

  TAiffMIDIChunk = class(TAiffDefinedChunk)
  private
    function GetMIDIData(index: Integer): Byte;
  protected
    FMIDIData: array of Byte;
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  public
    class function GetClassChunkName: TChunkName; override;
    property MIDIData[index: Integer]: Byte read GetMIDIData;
  end;

  TAiffAudioRecordingRecord = packed record
    AESChannelStatusData: array [0 .. 23] of AnsiChar;
  end;

  TAiffAudioRecordingChunk = class(TAiffFixedDefinedChunk) // 'AESD'
  private
    function GetAESChannelStatusData: AnsiString;
  public
    AudioRecordingRecord: TAiffAudioRecordingRecord;
    constructor Create; override;
    class function GetClassChunkSize: Cardinal; override;
    class function GetClassChunkName: TChunkName; override;

    property AESChannelStatusData: AnsiString read GetAESChannelStatusData;
  end;

  TAiffApplicationSpecificChunk = class(TAiffDefinedChunk) // 'APPL'
  private
    function GetApplicationSignature: AnsiString;
    function GetData(index: Integer): Byte;
    procedure CalculateChunkSize;
  protected
    FApplicationSignature: TChunkName;
    FApplicationData: AnsiString;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    class function GetClassChunkName: TChunkName; override;
    property ApplicationData[index: Integer]: Byte read GetData;

    property ApplicationSignature: AnsiString read GetApplicationSignature;
    property ApplicationDataAsString: AnsiString read FApplicationData;
  end;

  TAiffNameChunk = class(TAiffTextChunk)
  public
    class function GetClassChunkName: TChunkName; override;
  published
    property Name: AnsiString read FText;
  end;

  TAiffAuthorChunk = class(TAiffTextChunk)
  public
    class function GetClassChunkName: TChunkName; override;
  published
    property Author: AnsiString read FText;
  end;

  TAiffCopyrightChunk = class(TAiffTextChunk)
  public
    class function GetClassChunkName: TChunkName; override;
  published
    property Copyright: AnsiString read FText;
  end;

  TAiffAnnotationChunk = class(TAiffTextChunk)
  public
    class function GetClassChunkName: TChunkName; override;
  published
    property Annotation: AnsiString read FText;
  end;

  TAiffChunkScan = (acsName, acsAuthor, acsCopyright, acsMarker,
    acsAudioRecording, acsComment, acsInstrument);
  TAiffChunkScans = set of TAiffChunkScan;

  EAiffError = class(Exception);

  TFileAiff = class(TChunkedFile)
  private
    FIsCompressed: Boolean;
    FCommonChunk: TAiffCommonChunk;
    FCommentChunk: TAiffCommentChunk;
    FNameChunk: TAiffNameChunk;
    FAuthorChunk: TAiffAuthorChunk;
    FCopyrightChunk: TAiffCopyrightChunk;
    FAudioRecordingChunk: TAiffAudioRecordingChunk;
    FMarkerChunk: TAiffMarkerChunk;
    FInstrumentChunk: TAiffInstrumentChunk;
    FVersionChunk: TAiffFormatVersionChunk;
    FAiffChunkScans: TAiffChunkScans;
    FSsndChunk: TAiffSoundDataChunk;
    function GetAESChannelStatusData: AnsiString;
    function GetAIFFName: AnsiString;
    function GetAuthor: AnsiString;
    function GetCopyright: AnsiString;
    function GetDataSize: Cardinal;
    function GetBitsPerSample: Byte;
    function GetChannels: Cardinal;
    function GetSampleFrames: Cardinal;
    function GetSampleRate: Double;
    procedure WriteSSNDChunk(const Stream: TStream);
  protected
    procedure ReadAESDChunk(const Stream: TStream); virtual;
    procedure ReadAUTHChunk(const Stream: TStream); virtual;
    procedure ReadCOMMChunk(const Stream: TStream); virtual;
    procedure ReadCOMTChunk(const Stream: TStream); virtual;
    procedure ReadCOPYChunk(const Stream: TStream); virtual;
    procedure ReadFVERChunk(const Stream: TStream); virtual;
    procedure ReadINSTChunk(const Stream: TStream); virtual;
    procedure ReadMARKChunk(const Stream: TStream); virtual;
    procedure ReadNAMEChunk(const Stream: TStream); virtual;
    procedure ReadSSNDChunk(const Stream: TStream); virtual;
    procedure ReadUnknownChunk(const Stream: TStream); virtual;

    procedure ReadAndSkipSize(const Stream: TStream);
    procedure CheckHeader(const Stream: TStream);
    procedure ParseStream(const Stream: TStream);
  public
    constructor Create; override;

    // load/save stream
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    class function CanLoad(const Stream: TStream): Boolean;
    class function GetClassChunkName: TChunkName; override;

    property BitsPerSample: Byte read GetBitsPerSample;
    property DataSize: Cardinal read GetDataSize;
    property AiffChunkScans: TAiffChunkScans read FAiffChunkScans;

    property Name: AnsiString read GetAIFFName;
    property Author: AnsiString read GetAuthor;
    property Copyright: AnsiString read GetCopyright;
    property AESChannelStatusData: AnsiString read GetAESChannelStatusData;
  end;

implementation

uses
  CfeUtils;

resourcestring
  RCStrFORMChunkNotFound = 'This is not a FORM file!';
  RCStrFORMSizeMismatch = 'Filesize mismatch';
  RCStrAIFFChunkNotFound = 'This is not a AIFF file!';
  RCStrFMTChunkDublicate = 'One format chunk has already been found!';
  RCStrFACTChunkDublicate = 'One fact chunk has already been found!';
  RCStrDATAChunkDublicate = 'Only one data chunk supported!';
  RCStrOneVersionChunkOnly = 'Only one version chunk allowed';
  RCStrOneCommentChunkOnly = 'Only one comment chunk allowed';
  RCStrOneMarkerChunkOnly = 'Only one marker chunk allowed';
  RCStrOneInstrumentChunkOnly = 'Only one instrument chunk allowed';
  RCStrOneCopyrightChunkOnly = 'Only one copyright chunk allowed';
  RCStrOneNameChunkOnly = 'Only one name chunk allowed';
  RCStrOneAuthorChunkOnly = 'Only one author chunk allowed';
  RCStrOneAESChunkOnly = 'Only one audio recording chunk allowed';
  RCStrOneSsndChunkOnly = 'Only one ssnd chunk allowed';
  RCStrNoSoundData = 'No sound data information found!';

{ TAiffDefinedChunk }

constructor TAiffDefinedChunk.Create;
begin
  inherited;
  FChunkFlags := FChunkFlags + [cfReversedByteOrder, cfPadSize];
end;

{ TAiffFixedDefinedChunk }

constructor TAiffFixedDefinedChunk.Create;
begin
  inherited;
  FChunkFlags := FChunkFlags + [cfReversedByteOrder, cfPadSize];
end;

{ TAiffUnknownChunk }

constructor TAiffUnknownChunk.Create;
begin
  inherited;
  FChunkFlags := FChunkFlags + [cfReversedByteOrder, cfPadSize];
end;

{ TAiffTextChunk }

constructor TAiffTextChunk.Create;
begin
  inherited;
  FChunkFlags := FChunkFlags + [cfReversedByteOrder, cfPadSize];
end;

{ TAiffCommonChunk }

constructor TAiffCommonChunk.Create;
begin
  inherited;

  // set defaults
  with AIFFCommonRecord do
  begin
    Channels := 1; // one channel
    SampleRate := 44100; // 44.1 kHz (CD quality)
    SampleSize := 16; // 16bit
    SampleFrames := 0; // no data yet
  end;
  FCompressionType := ctNotAvailable;

  CalculateChunkSize;
end;

class function TAiffCommonChunk.GetClassChunkName: TChunkName;
begin
  Result := 'COMM';
end;

procedure TAiffCommonChunk.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TAiffCommonChunk then
    with TAiffCommonChunk(Dest) do
    begin
      AIFFCommonRecord := Self.AIFFCommonRecord;
      FCompressionType := Self.FCompressionType;
    end;
end;

procedure TAiffCommonChunk.CalculateChunkSize;
begin
  FChunkSize := SizeOf(AIFFCommonRecord);
  if FCompressionType <> ctNotAvailable then
    FChunkSize := FChunkSize + SizeOf(TChunkName) +
      Cardinal(Length(FCompressionName));
end;

procedure TAiffCommonChunk.LoadFromStream(Stream: TStream);
var
  CompTypeID: TChunkName;
  CompStrLen: Byte;
  CompString: string;
begin
  inherited;
  with Stream do
  begin
    // load basic header first
    Read(AIFFCommonRecord, SizeOf(TAiffCommonRecord));

    // flip header
    with AIFFCommonRecord do
    begin
      Flip16(Channels);
      Flip32(SampleFrames);
      Flip16(SampleSize);
      Flip80(SampleRate);
    end;

    // exit if no addition information are available
    if (FChunkSize = SizeOf(TAiffCommonRecord)) and not FForceReadCompression
    then
    begin
      Compression := ctNotAvailable;
      Exit;
    end;

    // read additional compression information
    Read(CompTypeID, SizeOf(TChunkName));
    if CompTypeID = 'NONE' then
      FCompressionType := ctNone
    else if CompTypeID = 'ALAW' then
      FCompressionType := ctALAW
    else if CompTypeID = 'alaw' then
      FCompressionType := ctALAW
    else if CompTypeID = 'ULAW' then
      FCompressionType := ctULAW
    else if CompTypeID = 'fl32' then
      FCompressionType := ctFL32
    else if CompTypeID = 'fl64' then
      FCompressionType := ctFL64
    else if CompTypeID = 'G722' then
      FCompressionType := ctG722
    else if CompTypeID = 'G726' then
      FCompressionType := ctG726
    else if CompTypeID = 'G728' then
      FCompressionType := ctG728
    else if CompTypeID = 'GSM ' then
      FCompressionType := ctGSM
    else if CompTypeID = 'ACE2' then
      FCompressionType := ctACE2
    else if CompTypeID = 'ACE8' then
      FCompressionType := ctACE8
    else if CompTypeID = 'MAC3' then
      FCompressionType := ctMACE3
    else if CompTypeID = 'MAC8' then
      FCompressionType := ctMACE6
    else
      FCompressionType := ctUnknown;

    Read(CompStrLen, 1);
    SetLength(CompString, CompStrLen);
    Read(CompString[1], CompStrLen);

    // eventually zero pad chunk
    if (CompStrLen mod 2) <> 0 then
      Position := Position + 1;
  end;
end;

procedure TAiffCommonChunk.SaveToStream(Stream: TStream);
var
  FlippedAIFFCommonRecord: TAiffCommonRecord;
  CompressionTypeID: TChunkName;
  CompressionNameLength: Byte;
begin
  CalculateChunkSize;
  inherited;
  with Stream do
  begin
    // save basic header first (need to be flipped first)
    FlippedAIFFCommonRecord := AIFFCommonRecord;
    with FlippedAIFFCommonRecord do
    begin
      Flip16(Channels);
      Flip32(SampleFrames);
      Flip16(SampleSize);
      Flip80(SampleRate);
    end;
    Write(FlippedAIFFCommonRecord, SizeOf(TAiffCommonRecord));

    // write compression info if necessary
    if FCompressionType <> ctNotAvailable then
    begin
      case FCompressionType of
        ctNone:
          CompressionTypeID := 'None';
        ctACE2:
          CompressionTypeID := 'ACE2';
        ctACE8:
          CompressionTypeID := 'ACE8';
        ctMACE3:
          CompressionTypeID := 'MAC3';
        ctMACE6:
          CompressionTypeID := 'MAC6';
        ctALAW:
          CompressionTypeID := 'ALAW';
        ctULAW:
          CompressionTypeID := 'ULAW';
        ctFL32:
          CompressionTypeID := 'fl32';
        ctFL64:
          CompressionTypeID := 'fl64';
        ctG722:
          CompressionTypeID := 'G722';
        ctG726:
          CompressionTypeID := 'G726';
        ctG728:
          CompressionTypeID := 'G728';
        ctGSM:
          CompressionTypeID := 'GSM ';
        ctUnknown:
          raise Exception.Create('Not supported');
      end;
      Write(CompressionTypeID, SizeOf(TChunkName));
      CompressionNameLength := Length(FCompressionName);
      Write(CompressionNameLength, SizeOf(CompressionNameLength));
      Write(FCompressionName[1], Length(FCompressionName));

      // eventually zero pad chunk
      if (CompressionNameLength mod 2) <> 0 then
        Position := Position + 1;
    end;
  end;
end;


{ TAiffFormChunk }

constructor TAiffFormChunk.Create;
begin
  inherited;
  with AIFFFormRecord do
  begin
    FormType := 'AIFF';
  end;
  StartAddress := @AIFFFormRecord;
end;

procedure TAiffFormChunk.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TAiffFormChunk then
  begin
    TAiffFormChunk(Dest).AIFFFormRecord := AIFFFormRecord;
  end;
end;

class function TAiffFormChunk.GetClassChunkName: TChunkName;
begin
  Result := 'FORM';
end;

class function TAiffFormChunk.GetClassChunkSize: Cardinal;
begin
  Result := SizeOf(TAiffFormRecord);
end;

function TAiffFormChunk.GetFormType: string;
begin
  Result := string(AIFFFormRecord.FormType);
end;


{ TAiffFormatVersionChunk }

constructor TAiffFormatVersionChunk.Create;
begin
  inherited;
  with AIFFFormatVersionRecord do
  begin
    TimeStamp := AIFCVersion1;
  end;
  StartAddress := @AIFFFormatVersionRecord;
end;

procedure TAiffFormatVersionChunk.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TAiffFormatVersionChunk then
  begin
    TAiffFormatVersionChunk(Dest).AIFFFormatVersionRecord :=
      AIFFFormatVersionRecord;
  end;
end;

class function TAiffFormatVersionChunk.GetClassChunkName: TChunkName;
begin
  Result := 'FVER';
end;

class function TAiffFormatVersionChunk.GetClassChunkSize: Cardinal;
begin
  Result := SizeOf(TAiffFormatVersionRecord);
end;


{ TAiffSoundDataChunk }

constructor TAiffSoundDataChunk.Create;
begin
  inherited;

  with FSoundDataRecord do
  begin
    Offset := 0;
    BlockSize := 0;
  end;

  FMemoryStream := TMemoryStream.Create;
end;

destructor TAiffSoundDataChunk.Destroy;
begin
  FMemoryStream.Free;

  inherited;
end;

class function TAiffSoundDataChunk.GetClassChunkName: TChunkName;
begin
  Result := 'SSND';
end;

procedure TAiffSoundDataChunk.LoadFromStream(Stream: TStream);
begin
  inherited;
  with Stream do
  begin
    // load basic header first
    Read(FSoundDataRecord, SizeOf(TAiffSoundDataRecord));

    // flip header
    with FSoundDataRecord do
    begin
      Flip32(Offset);
      Flip32(BlockSize);
    end;

    FMemoryStream.Clear;
    FMemoryStream.CopyFrom(Stream, FChunkSize - SizeOf(TAiffSoundDataRecord));
  end;
end;

procedure TAiffSoundDataChunk.SaveToStream(Stream: TStream);
var
  FlippedAIFFSoundDataRecord: TAiffSoundDataRecord;
begin
  CalculateChunkSize;

  inherited;

  with Stream do
  begin
    // save basic header first (need to be flipped first)
    FlippedAIFFSoundDataRecord := FSoundDataRecord;
    with FlippedAIFFSoundDataRecord do
    begin
      Flip32(Offset);
      Flip32(BlockSize);
    end;
    Write(FlippedAIFFSoundDataRecord, SizeOf(TAiffSoundDataRecord));

    FMemoryStream.Position := 0;
    FMemoryStream.SaveToStream(Stream);
  end;
end;

procedure TAiffSoundDataChunk.CalculateChunkSize;
begin
  FChunkSize := SizeOf(FSoundDataRecord);
end;

procedure TAiffSoundDataChunk.AssignTo(Dest: TPersistent);
begin
  inherited;

  if Dest is TAiffSoundDataChunk then
    TAiffSoundDataChunk(Dest).FSoundDataRecord := FSoundDataRecord;
end;


{ TAiffMarkerItem }

procedure TAiffMarkerItem.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TAiffMarkerItem then
    TAiffMarkerItem(Dest).MarkerRecord := MarkerRecord;
end;

function TAiffMarkerItem.GetDisplayName: string;
begin
  Result := FMarkerName;
end;

function TAiffMarkerItem.GetSize: Cardinal;
begin
  Result := SizeOf(TAiffMarkerRecord) + Length(FMarkerName) + 1;
end;

procedure TAiffMarkerItem.LoadFromStream(Stream: TStream);
var
  StringSize: Byte;
begin
  with Stream do
  begin
    // read marker header
    Read(MarkerRecord, SizeOf(TAiffMarkerRecord));

    Flip16(MarkerRecord.MarkerID);
    Flip16(MarkerRecord.Position);

    // now read the marker string
    Read(StringSize, SizeOf(Byte));

    SetLength(FMarkerName, StringSize);
    Read(FMarkerName[1], StringSize);

    // add pad byte if necessary
    if StringSize mod 2 = 0 then
      Position := Position + 1;
  end;
end;

procedure TAiffMarkerItem.SaveToStream(Stream: TStream);
var
  StringSize: Integer;
begin
  with Stream do
  begin
    // write marker header
    Write(MarkerRecord, SizeOf(TAiffMarkerRecord));

    // now write the marker string
    StringSize := Length(FMarkerName);
    Write(StringSize, SizeOf(Byte));
    Write(FMarkerName[1], StringSize);
  end;
end;

procedure TAiffMarkerItem.SetDisplayName(const Value: string);
begin
  FMarkerName := Value;
  inherited;
end;


{ TAiffMarkerChunk }

constructor TAiffMarkerChunk.Create;
begin
  inherited;
  FMarkers := TOwnedCollection.Create(Self, TAiffMarkerItem);
  CalculateChunkSize;
end;

destructor TAiffMarkerChunk.Destroy;
begin
  FreeAndNil(FMarkers);
  inherited;
end;

class function TAiffMarkerChunk.GetClassChunkName: TChunkName;
begin
  Result := 'MARK';
end;

function TAiffMarkerChunk.GetMarker(Index: Integer): TAiffMarkerItem;
begin
  if (Index < 0) or (Index >= FMarkers.Count) then
    raise Exception.Create('Index out of bounds');

  Result := TAiffMarkerItem(FMarkers.Items[Index]);
end;

function TAiffMarkerChunk.GetMarkerCount: Byte;
begin
  Result := FMarkers.Count;
end;

procedure TAiffMarkerChunk.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TAiffMarkerChunk then
    TAiffMarkerChunk(Dest).FMarkers.Assign(FMarkers);
end;

procedure TAiffMarkerChunk.CalculateChunkSize;
var
  i: Integer;
begin
  FChunkSize := SizeOf(Byte);
  for i := 0 to FMarkers.Count - 1 do
    FChunkSize := FChunkSize + TAiffMarkerItem(FMarkers.Items[i]).GetSize;
end;

procedure TAiffMarkerChunk.LoadFromStream(Stream: TStream);
var
  i: Integer;
  MarkerCount: Word;
begin
  inherited;
  with Stream do
  begin
    // load number of markers
    Read(MarkerCount, SizeOf(Word));
    Flip16(MarkerCount);
    Assert(MarkerCount < (FChunkSize div SizeOf(TAiffMarkerRecord)));

    // clear existing markers
    FMarkers.Clear;

    // load every single marker
    for i := 0 to MarkerCount - 1 do
      with TAiffMarkerItem(FMarkers.Add) do
        LoadFromStream(Stream);

  end;
end;

procedure TAiffMarkerChunk.SaveToStream(Stream: TStream);
var
  MarkerCount, i: Integer;
begin
  CalculateChunkSize;
  inherited;
  with Stream do
  begin
    // store number of markers
    MarkerCount := FMarkers.Count;
    Write(MarkerCount, SizeOf(Byte));

    for i := 0 to FMarkers.Count - 1 do
      TAiffMarkerItem(FMarkers.Items[i]).SaveToStream(Stream);
  end;
end;

{ TAiffCommentItem }

procedure TAiffCommentItem.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TAiffCommentItem then
    TAiffCommentItem(Dest).CommentRecord := CommentRecord;
end;

function TAiffCommentItem.GetDisplayName: string;
begin
  Result := FComment;
end;

function TAiffCommentItem.GetSize: Cardinal;
begin
  Result := SizeOf(TAiffCommentRecord) + Length(FComment) + 1;
end;

procedure TAiffCommentItem.LoadFromStream(Stream: TStream);
var
  StringSize: Integer;
begin
  with Stream do
  begin
    // read comment header
    Read(CommentRecord, SizeOf(TAiffCommentRecord));

    // now read the comment string
    Read(StringSize, SizeOf(Byte));
    SetLength(FComment, StringSize);
    Read(FComment[1], StringSize);
  end;
end;

procedure TAiffCommentItem.SaveToStream(Stream: TStream);
var
  StringSize: Integer;
begin
  with Stream do
  begin
    // write comment header
    Write(CommentRecord, SizeOf(TAiffCommentRecord));

    // now write the comment string
    StringSize := Length(FComment);
    Write(StringSize, SizeOf(Byte));
    Write(FComment[1], StringSize);
  end;
end;

procedure TAiffCommentItem.SetDisplayName(const Value: string);
begin
  FComment := Value;
  inherited;
end;


{ TAiffCommentChunk }

constructor TAiffCommentChunk.Create;
begin
  inherited;
  FComments := TOwnedCollection.Create(Self, TAiffCommentItem);
  CalculateChunkSize;
end;

destructor TAiffCommentChunk.Destroy;
begin
  FreeAndNil(FComments);
  inherited;
end;

procedure TAiffCommentChunk.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TAiffCommentChunk then
    TAiffCommentChunk(Dest).FComments.Assign(FComments);
end;

procedure TAiffCommentChunk.CalculateChunkSize;
var
  i: Integer;
begin
  FChunkSize := SizeOf(Byte);
  for i := 0 to FComments.Count - 1 do
    FChunkSize := FChunkSize + TAiffCommentItem(FComments.Items[i]).GetSize;
end;

class function TAiffCommentChunk.GetClassChunkName: TChunkName;
begin
  Result := 'COMT';
end;

function TAiffCommentChunk.GetComment(Index: Integer): TAiffCommentItem;
begin
  if (Index < 0) or (Index >= FComments.Count) then
    raise Exception.Create('Index out of bounds');

  Result := TAiffCommentItem(FComments.Items[Index]);
end;

function TAiffCommentChunk.GetCommentCount: Byte;
begin
  Result := FComments.Count;
end;

procedure TAiffCommentChunk.LoadFromStream(Stream: TStream);
var
  CommentCount, i: Integer;
begin
  inherited;
  with Stream do
  begin
    // load number of comments
    Read(CommentCount, SizeOf(Byte));

    // clear existing comments
    FComments.Clear;

    // load every single comment
    for i := 0 to CommentCount - 1 do
      with TAiffCommentItem(FComments.Add) do
        LoadFromStream(Stream);

    // set position to end of chunk
    Position := Position + FChunkSize - SizeOf(Byte);
  end;
end;

procedure TAiffCommentChunk.SaveToStream(Stream: TStream);
var
  CommentCount, i: Integer;
begin
  CalculateChunkSize;
  inherited;
  with Stream do
  begin
    // store number of comments
    CommentCount := FComments.Count;
    Write(CommentCount, SizeOf(Byte));

    for i := 0 to FComments.Count - 1 do
      TAiffCommentItem(FComments.Items[i]).SaveToStream(Stream);
  end;
end;

{ TAiffInstrumentChunk }

constructor TAiffInstrumentChunk.Create;
begin
  inherited;
  with FInstrumentRecord do
  begin
    BaseNote := 60;
    Detune := 0;
    LowNote := 0;
    HighNote := 127;
    LowVelocity := 1;
    HighVelocity := 127;
    Gain := 0;
  end;
  StartAddress := @FInstrumentRecord;
end;

procedure TAiffInstrumentChunk.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TAiffInstrumentChunk then
    TAiffInstrumentChunk(Dest).FInstrumentRecord := FInstrumentRecord;
end;

class function TAiffInstrumentChunk.GetClassChunkName: TChunkName;
begin
  Result := 'INST';
end;

class function TAiffInstrumentChunk.GetClassChunkSize: Cardinal;
begin
  Result := SizeOf(TAiffInstrumentRecord);
end;

{ TAiffMIDIChunk }

procedure TAiffMIDIChunk.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TAiffMIDIChunk then
  begin
    SetLength(TAiffMIDIChunk(Dest).FMIDIData, Length(FMIDIData));
    Move(FMIDIData[0], TAiffMIDIChunk(Dest).FMIDIData[0], Length(FMIDIData));
  end;
end;

class function TAiffMIDIChunk.GetClassChunkName: TChunkName;
begin
  Result := 'MIDI';
end;

function TAiffMIDIChunk.GetMIDIData(index: Integer): Byte;
begin
  if (index >= 0) and (index < Length(FMIDIData)) then
    Result := FMIDIData[index]
  else
    Result := 0;
end;

procedure TAiffMIDIChunk.LoadFromStream(Stream: TStream);
begin
  inherited;
  with Stream do
  begin
    SetLength(FMIDIData, FChunkSize);
    Read(FMIDIData[0], FChunkSize);
  end;
end;

procedure TAiffMIDIChunk.SaveToStream(Stream: TStream);
begin
  FChunkSize := Length(FMIDIData);
  inherited;
  with Stream do
  begin
    Write(FMIDIData[0], FChunkSize);
  end;
end;


{ TAiffAudioRecordingChunk }

constructor TAiffAudioRecordingChunk.Create;
begin
  inherited;
  StartAddress := @AudioRecordingRecord;
  AudioRecordingRecord.AESChannelStatusData := '';
end;

function TAiffAudioRecordingChunk.GetAESChannelStatusData: AnsiString;
begin
  SetLength(Result, 24);
  Move(AudioRecordingRecord.AESChannelStatusData[0], Result[1], 24);
end;

class function TAiffAudioRecordingChunk.GetClassChunkName: TChunkName;
begin
  Result := 'AESD';
end;

class function TAiffAudioRecordingChunk.GetClassChunkSize: Cardinal;
begin
  Result := SizeOf(TAiffAudioRecordingRecord);
end;


{ TAiffApplicationSpecificChunk }

constructor TAiffApplicationSpecificChunk.Create;
begin
  inherited;
  // default the signature assotiated with this project
  FApplicationSignature := 'DAVD';
end;

procedure TAiffApplicationSpecificChunk.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TAiffApplicationSpecificChunk then
  begin
    TAiffApplicationSpecificChunk(Dest).FApplicationSignature :=
      FApplicationSignature;
    SetLength(TAiffApplicationSpecificChunk(Dest).FApplicationData,
      Length(FApplicationData));
    Move(FApplicationData[1], TAiffApplicationSpecificChunk(Dest)
      .FApplicationData[1], Length(FApplicationData));
  end;
end;

procedure TAiffApplicationSpecificChunk.CalculateChunkSize;
begin
  FChunkSize := Length(FApplicationData) + SizeOf(TChunkName);
end;

function TAiffApplicationSpecificChunk.GetApplicationSignature: AnsiString;
begin
  Result := AnsiString(FApplicationSignature);
end;

class function TAiffApplicationSpecificChunk.GetClassChunkName: TChunkName;
begin
  Result := 'APPL';
end;

function TAiffApplicationSpecificChunk.GetData(index: Integer): Byte;
begin
  if (Index >= 0) and (Index < Length(FApplicationData)) then
    Result := Byte(FApplicationData[Index + 1])
  else
    Result := 0;
end;

procedure TAiffApplicationSpecificChunk.LoadFromStream(Stream: TStream);
begin
  inherited;
  with Stream do
  begin
    // read application signature
    Read(FApplicationSignature, SizeOf(TChunkName));

    // read application data
    SetLength(FApplicationData, FChunkSize - SizeOf(TChunkName));
    Read(FApplicationData[1], FChunkSize);
  end;
end;

procedure TAiffApplicationSpecificChunk.SaveToStream(Stream: TStream);
begin
  CalculateChunkSize;
  inherited;
  with Stream do
  begin
    // write application signature
    Write(FApplicationSignature, SizeOf(TChunkName));

    // write application data
    Write(FApplicationData[1], FChunkSize - SizeOf(TChunkName));
  end;
end;

{ TAiffNameChunk }

class function TAiffNameChunk.GetClassChunkName: TChunkName;
begin
  Result := 'NAME';
end;

{ TAiffAuthorChunk }

class function TAiffAuthorChunk.GetClassChunkName: TChunkName;
begin
  Result := 'AUTH';
end;

{ TAiffCopyrightChunk }

class function TAiffCopyrightChunk.GetClassChunkName: TChunkName;
begin
  Result := '(c) ';
end;

{ TAiffAnnotationChunk }

class function TAiffAnnotationChunk.GetClassChunkName: TChunkName;
begin
  Result := 'ANNO';
end;

{ TFileAiff }

constructor TFileAiff.Create;
begin
  inherited;
  FChunkFlags := FChunkFlags + [cfReversedByteOrder, cfPadSize];
  FCommonChunk := TAiffCommonChunk.Create;
  FAiffChunkScans := [acsName, acsAuthor, acsCopyright, acsMarker, acsComment,
    acsInstrument];
end;

class function TFileAiff.CanLoad(const Stream: TStream): Boolean;
var
  ChunkName: TChunkName;
  ChunkSize: Cardinal;
  OldPosition: Cardinal;
begin
  Result := False;

  // store old position
  OldPosition := Stream.Position;

  with Stream do
    try
      // check minimum file size
      if Size < 12 then
        Exit;

      // check whether file is a resource interchange file format ('FORM')
      Read(ChunkName, 4);
      if ChunkName <> 'FORM' then
        Exit;

      // check whether the real file size match the filesize stored inside the FORM chunk
      Read(ChunkSize, 4);
      Flip32(ChunkSize);
      if (ChunkSize > ((Size + 1) shr 1) shl 1 - Position) and
        not(ChunkSize = $FFFFFFFF) then
        Exit;

      // now specify the FORM file to be a AIFF or AIFC file
      Read(ChunkName, 4);
      if (ChunkName <> 'AIFF') and (ChunkName <> 'AIFC') then
        Exit;

      Result := True;
    finally
      // restore old position
      Position := OldPosition;
    end;
end;

function TFileAiff.GetAuthor: AnsiString;
begin
  if Assigned(FAuthorChunk) then
    Result := FAuthorChunk.Author
  else
    Result := '';
end;

function TFileAiff.GetBitsPerSample: Byte;
begin
  Result := FCommonChunk.SampleSize;
end;

function TFileAiff.GetChannels: Cardinal;
begin
  Result := FCommonChunk.Channels;
end;

class function TFileAiff.GetClassChunkName: TChunkName;
begin
  Result := 'FORM';
end;

function TFileAiff.GetCopyright: AnsiString;
begin
  if Assigned(FCopyrightChunk) then
    Result := FCopyrightChunk.Copyright
  else
    Result := '';
end;

function TFileAiff.GetDataSize: Cardinal;
begin
  Result := FCommonChunk.SampleFrames * Cardinal(FCommonChunk.Channels) *
    Cardinal((FCommonChunk.SampleSize + 7) div 8);
end;

function TFileAiff.GetAESChannelStatusData: AnsiString;
begin
  if Assigned(FAudioRecordingChunk) then
    Result := FAudioRecordingChunk.AESChannelStatusData
  else
    Result := '';
end;

function TFileAiff.GetAIFFName: AnsiString;
begin
  if Assigned(FNameChunk) then
    Result := FNameChunk.Name
  else
    Result := '';
end;

function TFileAiff.GetSampleFrames: Cardinal;
begin
  Result := FCommonChunk.SampleFrames;
end;

function TFileAiff.GetSampleRate: Double;
begin
  Result := FCommonChunk.SampleRate;
end;

procedure TFileAiff.CheckHeader(const Stream: TStream);
var
  ChunkName: TChunkName;
begin
  with Stream do
  begin
    // check whether file is a resource interchange file format ('FORM')
    Read(ChunkName, 4);
    if ChunkName <> 'FORM' then
      raise EAIFFError.Create(RCStrFORMChunkNotFound);

    // check whether the real file size match the filesize stored inside the FORM chunk
    Read(FChunkSize, 4);
    Flip32(FChunkSize);
    if (FChunkSize > ((Size + 1) shr 1) shl 1 - Position) and
      not(FChunkSize = $FFFFFFFF) then
      raise EAIFFError.Create(RCStrFORMSizeMismatch);

    // now specify the FORM file to be a AIFC/AIFF file
    Read(ChunkName, 4);
    FIsCompressed := ChunkName = 'AIFC';
    if (ChunkName <> 'AIFF') and (ChunkName <> 'AIFC') then
      raise EAIFFError.Create(RCStrAIFFChunkNotFound);
  end;
end;

procedure TFileAiff.ParseStream(const Stream: TStream);
var
  ChunkName: TChunkName;
  ChunkEnd: Cardinal;
begin
  with Stream do
  begin
    // Remove existing optional chunk
    if Assigned(FCommentChunk) then
      FreeAndNil(FCommentChunk);
    if Assigned(FMarkerChunk) then
      FreeAndNil(FMarkerChunk);
    if Assigned(FInstrumentChunk) then
      FreeAndNil(FInstrumentChunk);
    if Assigned(FVersionChunk) then
      FreeAndNil(FVersionChunk);
    if Assigned(FNameChunk) then
      FreeAndNil(FNameChunk);
    if Assigned(FAuthorChunk) then
      FreeAndNil(FAuthorChunk);
    if Assigned(FCopyrightChunk) then
      FreeAndNil(FCopyrightChunk);
    if Assigned(FAudioRecordingChunk) then
      FreeAndNil(FAudioRecordingChunk);

    Assert(Position = 12);
    ChunkEnd := Position + FChunkSize - 4;

    // start parsing here
    while Stream.Position < ChunkEnd do
    begin
      // read chunk name
      Read(ChunkName, 4);

      // set position to chunk start
      Position := Position - 4;

      if ChunkName = 'FVER' then
        ReadFVERChunk(Stream)
      else if ChunkName = 'COMM' then
        ReadCOMMChunk(Stream)
      else if ChunkName = 'SSND' then
        ReadSSNDChunk(Stream)
      else if ChunkName = 'MARK' then
        ReadMARKChunk(Stream)
      else if ChunkName = 'COMT' then
        ReadCOMTChunk(Stream)
      else if ChunkName = 'INST' then
        ReadINSTChunk(Stream)
      else if ChunkName = 'AESD' then
        ReadAESDChunk(Stream)
      else if ChunkName = 'NAME' then
        ReadNAMEChunk(Stream)
      else if ChunkName = 'AUTH' then
        ReadAUTHChunk(Stream)
      else if ChunkName = '(c) ' then
        ReadCOPYChunk(Stream)
      else
        ReadUnknownChunk(Stream);
    end;

    Assert(Position = ChunkEnd);

    if (FCommonChunk.SampleFrames > 0) then
      if not Assigned(FSsndChunk) then
        raise EAIFFError.Create(RCStrNoSoundData)
  end;
end;

procedure TFileAiff.ReadFVERChunk(const Stream: TStream);
begin
  with Stream do
  begin
    if Assigned(FVersionChunk) then
      raise EAIFFError.Create(RCStrOneVersionChunkOnly);

    FVersionChunk := TAiffFormatVersionChunk.Create;
    FVersionChunk.LoadFromStream(Stream);
    AddChunk(FVersionChunk);
  end;
end;

procedure TFileAiff.ReadCOMMChunk(const Stream: TStream);
begin
  with Stream do
  begin
    // load common chunk
    FCommonChunk.ForceReadCompression := FIsCompressed;
    FCommonChunk.LoadFromStream(Stream);
    AddChunk(FCommonChunk);
  end;
end;

procedure TFileAiff.ReadSSNDChunk(const Stream: TStream);
var
  DataSize: Cardinal;
begin
  with Stream do
  begin
    if Assigned(FSsndChunk) then
      raise EAIFFError.Create(RCStrOneSsndChunkOnly);
    FSsndChunk := TAiffSoundDataChunk.Create;
    FSsndChunk.LoadFromStream(Stream);
    AddChunk(FSsndChunk);
  end;
end;

procedure TFileAiff.ReadCOMTChunk(const Stream: TStream);
begin
  with Stream do
  begin
    if Assigned(FCommentChunk) then
      raise EAIFFError.Create(RCStrOneCommentChunkOnly);

    if acsComment in FAiffChunkScans then
    begin
      // load comment chunk
      FCommentChunk := TAiffCommentChunk.Create;
      FCommentChunk.LoadFromStream(Stream);
      AddChunk(FCommentChunk);
    end
    else
      ReadAndSkipSize(Stream);
  end;
end;

procedure TFileAiff.ReadMARKChunk(const Stream: TStream);
begin
  with Stream do
  begin
    if Assigned(FMarkerChunk) then
      raise EAIFFError.Create(RCStrOneMarkerChunkOnly);

    if acsMarker in FAiffChunkScans then
    begin
      // load marker chunk
      FMarkerChunk := TAiffMarkerChunk.Create;
      FMarkerChunk.LoadFromStream(Stream);
      AddChunk(FMarkerChunk);
    end
    else
      ReadAndSkipSize(Stream);
  end;
end;

procedure TFileAiff.ReadINSTChunk(const Stream: TStream);
begin
  with Stream do
  begin
    if Assigned(FInstrumentChunk) then
      raise EAIFFError.Create(RCStrOneInstrumentChunkOnly);

    if acsInstrument in FAiffChunkScans then
    begin
      // load instrument chunk
      FInstrumentChunk := TAiffInstrumentChunk.Create;
      FInstrumentChunk.LoadFromStream(Stream);
      AddChunk(FInstrumentChunk);
    end
    else
      ReadAndSkipSize(Stream);
  end;
end;

procedure TFileAiff.ReadAESDChunk(const Stream: TStream);
begin
  with Stream do
  begin
    if Assigned(FAudioRecordingChunk) then
      raise EAIFFError.Create(RCStrOneAESChunkOnly);

    if acsAudioRecording in FAiffChunkScans then
    begin
      // load name chunk
      FAudioRecordingChunk := TAiffAudioRecordingChunk.Create;
      FAudioRecordingChunk.LoadFromStream(Stream);
      AddChunk(FAudioRecordingChunk);
    end
    else
      ReadAndSkipSize(Stream);
  end;
end;

procedure TFileAiff.ReadNAMEChunk(const Stream: TStream);
begin
  with Stream do
  begin
    if Assigned(FNameChunk) then
      raise EAIFFError.Create(RCStrOneNameChunkOnly);

    if acsName in FAiffChunkScans then
    begin
      // load name chunk
      FNameChunk := TAiffNameChunk.Create;
      FNameChunk.LoadFromStream(Stream);
      AddChunk(FNameChunk);
    end
    else
      ReadAndSkipSize(Stream);
  end;
end;

procedure TFileAiff.ReadAUTHChunk(const Stream: TStream);
begin
  with Stream do
  begin
    if Assigned(FAuthorChunk) then
      raise EAIFFError.Create(RCStrOneAuthorChunkOnly);

    if acsAuthor in FAiffChunkScans then
    begin
      // load author chunk
      FAuthorChunk := TAiffAuthorChunk.Create;
      FAuthorChunk.LoadFromStream(Stream);
      AddChunk(FAuthorChunk);
    end
    else
      ReadAndSkipSize(Stream);
  end;
end;

procedure TFileAiff.ReadCOPYChunk(const Stream: TStream);
begin
  with Stream do
  begin
    if Assigned(FCopyrightChunk) then
      raise EAIFFError.Create(RCStrOneCopyrightChunkOnly);

    if acsCopyright in FAiffChunkScans then
    begin
      // load comment chunk
      FCopyrightChunk := TAiffCopyrightChunk.Create;
      FCopyrightChunk.LoadFromStream(Stream);
      AddChunk(FCopyrightChunk);
    end
    else
      ReadAndSkipSize(Stream);
  end;
end;

procedure TFileAiff.ReadUnknownChunk(const Stream: TStream);
var
  UnknownChunk: TAiffUnknownChunk;
begin
  with Stream do
    begin
      UnknownChunk := TAiffUnknownChunk.Create;
      UnknownChunk.LoadFromStream(Stream);
      AddChunk(UnknownChunk);
    end;
end;

procedure TFileAiff.ReadAndSkipSize(const Stream: TStream);
var
  ChunkSize: Cardinal;
begin
  with Stream do
  begin
    Read(ChunkSize, SizeOf(Cardinal));
    Position := Position + ChunkSize;
  end;
end;

procedure TFileAiff.LoadFromStream(Stream: TStream);
begin
  CheckHeader(Stream);
  ParseStream(Stream);
end;

procedure TFileAiff.SaveToStream(Stream: TStream);
var
  ChunkName: TChunkName;
  ChunkStart: Cardinal;
  ChunkSize: Cardinal;
  TempSize: Cardinal;
  Index: Integer;
begin
  with Stream do
  begin
    // Store chunk start position, just in case the stream position is not 0;
    ChunkStart := Position;

    // first write 'FORM' (resource interchange file format)
    ChunkName := 'FORM';
    Write(ChunkName, 4);

    // write dummy filesize yet, since final size is still unknown
    ChunkSize := $FFFFFFFF;
    Write(ChunkSize, 4);

    // now specify the FORM file to be an AIFF file
    ChunkName := 'AIFF';
    Write(ChunkName, 4);

    for Index := 0 to FChunkList.Count - 1 do
      FChunkList[Index].SaveToStream(Stream);

    // finally write filesize
    ChunkSize := Position - (ChunkStart + 8);
    Position := ChunkStart + 4;
    TempSize := ChunkSize;
    Flip32(TempSize);
    Write(TempSize, 4);

    // Reset Position to end of Stream;
    Position := ChunkStart + ChunkSize;
  end;
end;

procedure TFileAiff.WriteSSNDChunk(const Stream: TStream);
var
  ChunkName: TChunkName;
  ChunkSize: Cardinal;
const
  CZero: Cardinal = 0;
begin
  with Stream do
  begin
    // write 'data' chunk name
    ChunkName := 'SSND';
    Write(ChunkName, 4);

    // write chunk size
    ChunkSize := 8 + DataSize;
    Flip32(ChunkSize);
    Write(ChunkSize, 4);

    Write(CZero, 4); // offset
    Write(CZero, 4); // block align
  end;
end;

end.
