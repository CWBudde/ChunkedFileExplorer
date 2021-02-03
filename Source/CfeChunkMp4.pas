unit CfeChunkMp4;

interface

uses
  Classes, SysUtils, Contnrs, CfeChunkCommon;

type
  TMp4DefinedChunk = class(TDefinedChunk)
  public
    constructor Create; override;
  end;

  TMp4FixedDefinedChunk = class(TFixedDefinedChunk)
  public
    constructor Create; override;
  end;

  TMp4UnknownChunk = class(TUnknownChunk)
  public
    constructor Create; override;
  end;

  TMp4ContainerChunk = class(TChunkContainer)
  public
    constructor Create; override;
  end;

  TMp4TextChunk = class(TCustomTextChunk)
  public
    constructor Create; override;
  end;

  TMp4FileTypeChunk = class(TMp4DefinedChunk)
  private
    FMajorBrand: TChunkName;
    FMinorVersion: Integer;
    FCompatibleBrands: array of TChunkName;
    function GetCompatibleBrandsAsString: String;
  public
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    class function GetClassChunkName: TChunkName; override;

    property MajorBrand: TChunkName read FMajorBrand;
    property MinorVersion: Integer read FMinorVersion;
    property CompatibleBrandsAsString: String read GetCompatibleBrandsAsString;
  end;

  TMp4FreeChunk = class(TMp4DefinedChunk)
  public
    class function GetClassChunkName: TChunkName; override;
  end;

  TMp4MovieDataChunk = class(TMp4DefinedChunk)
  private
    FStream: TStream;
    procedure AssignTo(Dest: TPersistent); override;
    function GetChunkSize: Cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  end;

  TMp4MovieHeaderChunk = class(TMp4DefinedChunk)
  private
    FVersion: Byte;
    FFlags: array [0..2] of Byte;
    FCreationTime: Cardinal;
    FModificationTime: Cardinal;
    FTimeScale: Cardinal;
    FDuration: Cardinal;
    FPreferredRate: Cardinal;
    FPreferredVolume: Word;
    FReserved: array [0..9] of Byte;
    FMatrixStructure: array [0..8] of Integer;
    FPreviewTime: Cardinal;
    FPreviewDuration: Cardinal;
    FPosterTime: Cardinal;
    FSelectionTime: Cardinal;
    FSelectionDuration: Cardinal;
    FCurrentTime: Cardinal;
    FNextTrackId: Cardinal;
    function GetCreationTime: TDateTime;
    function GetDuration: Single;
    function GetModificationTime: TDateTime;
    function GetPreferredRate: Single;
    function GetPreferredVolume: Single;
    function GetTimeScale: Single;
  public
    constructor Create; override;

    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property CreationTime: TDateTime read GetCreationTime;
    property ModificationTime: TDateTime read GetModificationTime;
    property TimeScale: Single read GetTimeScale;
    property Duration: Single read GetDuration;
    property PreferredRate: Single read GetPreferredRate;
    property PreferredVolume: Single read GetPreferredVolume;
  end;

  TMp4TrackHeaderChunk = class(TMp4DefinedChunk)
  private
    FVersion: Byte;
    FFlags: array [0..2] of Byte;
    FCreationTime: Cardinal;
    FModificationTime: Cardinal;
    FTrackID: Cardinal;
    FReservedA: Cardinal;
    FDuration: Cardinal;
    FReservedB: array [0..7] of Byte;
    FLayer: Word;
    FAlternateGroup: Word;
    FVolume: Word;
    FReservedC: Word;
    FMatrixStructure: array [0..8] of Integer;
    FTrackWidth: Cardinal;
    FTrackHeight: Cardinal;
    function GetCreationTime: TDateTime;
    function GetModificationTime: TDateTime;
  public
    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property CreationTime: TDateTime read GetCreationTime;
    property ModificationTime: TDateTime read GetModificationTime;
  end;

  TMp4EditListEntry = class
  private
    FTrackDuration: Cardinal;
    FMediaTime: Cardinal;
    FMediaRate: Cardinal;
  public
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
  end;

  TMp4EditListEntryList = class(TObjectList)
  protected
    function GetItem(Index: Integer): TMp4EditListEntry;
    procedure SetItem(Index: Integer; AChunk: TMp4EditListEntry);
  public
    function Add(AChunk: TMp4EditListEntry): Integer;
    function Extract(Item: TMp4EditListEntry): TMp4EditListEntry;
    function Remove(AChunk: TMp4EditListEntry): Integer;
    function IndexOf(AChunk: TMp4EditListEntry): Integer;
    procedure Insert(Index: Integer; AChunk: TMp4EditListEntry);
    function First: TMp4EditListEntry;
    function Last: TMp4EditListEntry;

    property Items[Index: Integer]: TMp4EditListEntry read GetItem write SetItem; default;
  end;

  TMp4EditListChunk = class(TMp4DefinedChunk)
  private
    FVersion: Byte;
    FFlags: array [0..2] of Byte;
    FNumberOfEntries: Cardinal;
    FList: TMp4EditListEntryList;
  public
    constructor Create; override;
    destructor Destroy; override;

    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  end;

  TMp4EditChunk = class(TMp4ContainerChunk)
  public
    constructor Create; override;

    class function GetClassChunkName: TChunkName; override;
  end;

  TMp4MediaHeaderChunk = class(TMp4DefinedChunk)
  private
    FVersion: Byte;
    FFlags: array [0..2] of Byte;
    FCreationTime: Cardinal;
    FModificationTime: Cardinal;
    FTimeScale: Cardinal;
    FDuration: Cardinal;
    FLanguage: Word;
    FQuality: Word;
    function GetCreationTime: TDateTime;
    function GetDuration: Single;
    function GetModificationTime: TDateTime;
    function GetTimeScale: Single;
  public
    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property CreationTime: TDateTime read GetCreationTime;
    property ModificationTime: TDateTime read GetModificationTime;
    property TimeScale: Single read GetTimeScale;
    property Duration: Single read GetDuration;
  end;

  TMp4HandlerReferenceChunk = class(TMp4DefinedChunk)
  private
    FVersion: Byte;
    FFlags: array [0..2] of Byte;
    FComponentType: TChunkName;
    FComponentSubType: TChunkName;
    FComponentManufacturer: TChunkName;
    FComponentFlags: array [0..3] of Byte;
    FComponentFlagMask: array [0..3] of Byte;
    FComponentName: AnsiString;
  public
    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property ComponentType: TChunkName read FComponentType;
    property ComponentSubType: TChunkName read FComponentSubType;
    property ComponentManufacturer: TChunkName read FComponentManufacturer;
  end;

  TMp4VideoMediaInformationChunk = class(TMp4DefinedChunk)
  private
    FVersion: Byte;
    FFlags: array [0..2] of Byte;
    FGraphicsMode: Word;
    FOpColor: array [0..2] of Word;
  public
    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property GraphicsMode: Word read FGraphicsMode;
//    property FOpColor: array [0..2] of Word read FOpColor;
  end;

  TMp4SoundMediaInformationChunk = class(TMp4DefinedChunk)
  private
    FVersion: Byte;
    FFlags: array [0..2] of Byte;
    FBalance: Word;
    FReserved: Word;
  public
    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property Balance: Word read FBalance;
  end;

  TMp4BaseMediaInformationChunk = class(TMp4DefinedChunk)
  private
    FVersion: Byte;
    FFlags: array [0..2] of Byte;
    FGraphicsMode: Word;
    FOpColor: array [0..2] of Word;
    FBalance: Word;
    FReserved: Word;
  public
    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property GraphicsMode: Word read FGraphicsMode;
//    property FOpColor: array [0..2] of Word read FOpColor;
    property Balance: Word read FBalance;
  end;

  TMp4DataReferenceChunk = class(TMp4ContainerChunk)
  private
    FVersion: Byte;
    FFlags: array [0..2] of Byte;
    FNumberOfEntries: Cardinal;
  public
    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  end;

  TMp4ElementaryStreamDescriptorChunk = class(TMp4DefinedChunk)
  private
    FBitStream: TMemoryStream;
  public
    constructor Create; override;
    destructor Destroy; override;

    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  end;

  TMp4DataInformationChunk = class(TMp4ContainerChunk)
  public
    constructor Create; override;

    class function GetClassChunkName: TChunkName; override;
  end;

  TMp4Mp4aChunk = class(TMp4ContainerChunk)
  private
    FUnknownA: array [0..11] of Byte;
    FDataReferenceIndex: Cardinal;
    FUnknownB: array [0..7] of Byte;
    FChannelCount: Cardinal;
    FSampleSize: Cardinal;
    FUnknownC: array [0..1] of Byte;
    FSampleRate: Cardinal;
    FUnknownD: array [0..1] of Byte;
  public
    constructor Create; override;

    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  end;

  TMp4SampleDescriptionChunk = class(TMp4ContainerChunk)
  private
    FVersion: Byte;
    FFlags: array [0..2] of Byte;
    FNumberOfEntries: Cardinal;
  public
    constructor Create; override;

    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  end;

  TMp4SampleTableEntry = class
  private
    FSampleCount: Cardinal;
    FSampleDuration: Cardinal;
  public
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
  end;

  TMp4SampleTableEntryList = class(TObjectList)
  protected
    function GetItem(Index: Integer): TMp4SampleTableEntry;
    procedure SetItem(Index: Integer; AChunk: TMp4SampleTableEntry);
  public
    function Add(AChunk: TMp4SampleTableEntry): Integer;
    function Extract(Item: TMp4SampleTableEntry): TMp4SampleTableEntry;
    function Remove(AChunk: TMp4SampleTableEntry): Integer;
    function IndexOf(AChunk: TMp4SampleTableEntry): Integer;
    procedure Insert(Index: Integer; AChunk: TMp4SampleTableEntry);
    function First: TMp4SampleTableEntry;
    function Last: TMp4SampleTableEntry;

    property Items[Index: Integer]: TMp4SampleTableEntry read GetItem write SetItem; default;
  end;

  TMp4TimeToSampleChunk = class(TMp4DefinedChunk)
  private
    FVersion: Byte;
    FFlags: array [0..2] of Byte;
    FNumberOfEntries: Cardinal;
    FSampleTable: TMp4SampleTableEntryList;
  public
    constructor Create; override;
    destructor Destroy; override;

    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  end;

  TMp4SampleToChunkEntry = class
  private
    FFirstChunk: Cardinal;
    FSamplesPerChunk: Cardinal;
    FSampleDescription: Cardinal;
  public
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
  end;

  TMp4SampleToChunkEntryList = class(TObjectList)
  protected
    function GetItem(Index: Integer): TMp4SampleToChunkEntry;
    procedure SetItem(Index: Integer; AChunk: TMp4SampleToChunkEntry);
  public
    function Add(AChunk: TMp4SampleToChunkEntry): Integer;
    function Extract(Item: TMp4SampleToChunkEntry): TMp4SampleToChunkEntry;
    function Remove(AChunk: TMp4SampleToChunkEntry): Integer;
    function IndexOf(AChunk: TMp4SampleToChunkEntry): Integer;
    procedure Insert(Index: Integer; AChunk: TMp4SampleToChunkEntry);
    function First: TMp4SampleToChunkEntry;
    function Last: TMp4SampleToChunkEntry;

    property Items[Index: Integer]: TMp4SampleToChunkEntry read GetItem write SetItem; default;
  end;

  TMp4SampleToChunkChunk = class(TMp4DefinedChunk)
  private
    FVersion: Byte;
    FFlags: array [0..2] of Byte;
    FNumberOfEntries: Cardinal;
    FSampleToChunkTable: TMp4SampleToChunkEntryList;
  public
    constructor Create; override;
    destructor Destroy; override;

    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  end;

  TMp4SampleSizeChunk = class(TMp4DefinedChunk)
  private
    FVersion: Byte;
    FFlags: array [0..2] of Byte;
    FSampleSize: Cardinal;
    FNumberOfEntries: Cardinal;
    FSampleSizes: array of Cardinal;
  public
    constructor Create; override;

    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  end;

  TMp4ChunkOffsetChunk = class(TMp4DefinedChunk)
  private
    FVersion: Byte;
    FFlags: array [0..2] of Byte;
    FNumberOfEntries: Cardinal;
    FChunkOffsetTable: array of Int64;
  public
    constructor Create; override;

    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  end;

  TMp4SampleGroupDescriptionChunk = class(TMp4DefinedChunk)
  private
    FVersion: Byte;
    FFlags: array [0..2] of Byte;
    FGroupingType: TChunkName;
    FDefaultLength: Cardinal;
    FEntryCount: Cardinal;
    FPayload: array of SmallInt;
  public
    constructor Create; override;

    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property DefaultLength: Cardinal read FDefaultLength;
    property EntryCount: Cardinal read FEntryCount;
  end;

  TMp4SampleToGroupEntry = class
  private
    FSampleCount: Cardinal;
    FGroupDescriptionIndex: Cardinal;
  public
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);

    property SampleCount: Cardinal read FSampleCount write FSampleCount;
    property GroupDescriptionIndex: Cardinal read FGroupDescriptionIndex write FGroupDescriptionIndex;
  end;

  TMp4SampleToGroupEntryList = class(TObjectList)
  protected
    function GetItem(Index: Integer): TMp4SampleToGroupEntry;
    procedure SetItem(Index: Integer; AChunk: TMp4SampleToGroupEntry);
  public
    function Add(AChunk: TMp4SampleToGroupEntry): Integer;
    function Extract(Item: TMp4SampleToGroupEntry): TMp4SampleToGroupEntry;
    function Remove(AChunk: TMp4SampleToGroupEntry): Integer;
    function IndexOf(AChunk: TMp4SampleToGroupEntry): Integer;
    procedure Insert(Index: Integer; AChunk: TMp4SampleToGroupEntry);
    function First: TMp4SampleToGroupEntry;
    function Last: TMp4SampleToGroupEntry;

    property Items[Index: Integer]: TMp4SampleToGroupEntry read GetItem write SetItem; default;
  end;

  TMp4SampleToGroupChunk = class(TMp4DefinedChunk)
  private
    FVersion: Byte;
    FFlags: array [0..2] of Byte;
    FGroupingType: TChunkName;
    FEntryCount: Cardinal;
    FTableData: TMp4SampleToGroupEntryList;
  public
    constructor Create; override;
    destructor Destroy; override;

    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property GroupingType: TChunkName read FGroupingType;
    property TableData: TMp4SampleToGroupEntryList read FTableData;
  end;

  TMp4SampleTableChunk = class(TMp4ContainerChunk)
  public
    constructor Create; override;

    class function GetClassChunkName: TChunkName; override;
  end;

  TMp4MediaInformationChunk = class(TMp4ContainerChunk)
  public
    constructor Create; override;

    class function GetClassChunkName: TChunkName; override;
  end;

  TMp4MediaChunk = class(TMp4ContainerChunk)
  public
    constructor Create; override;

    class function GetClassChunkName: TChunkName; override;
  end;

  TMp4TrackChunk = class(TMp4ContainerChunk)
  public
    constructor Create; override;

    class function GetClassChunkName: TChunkName; override;
  end;

  TMp4DataChunk = class(TMp4DefinedChunk)
  private
    FType: Cardinal;
    FLocale: Cardinal;
    FData: AnsiString;
  public
    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property DataAsString: AnsiString read FData;
    property &Type: Cardinal read FType;
    property Locale: Cardinal read FLocale;
  end;

  TMp4CustomItemListChunk = class(TMp4ContainerChunk)
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  TMp4ItemListArtChunk = class(TMp4CustomItemListChunk)
  public
    class function GetClassChunkName: TChunkName; override;
  end;

  TMp4ItemListTooChunk = class(TMp4CustomItemListChunk)
  public
    class function GetClassChunkName: TChunkName; override;
  end;

  TMp4ItemListChunk = class(TMp4ContainerChunk)
  public
    constructor Create; override;

    class function GetClassChunkName: TChunkName; override;
  end;

  TMp4MetaChunk = class(TMp4ContainerChunk)
  private
    FUnknown: Cardinal;
  public
    constructor Create; override;

    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  end;

  TMp4UserDataChunk = class(TMp4ContainerChunk)
  public
    constructor Create; override;

    class function GetClassChunkName: TChunkName; override;
  end;

  TMp4MoovChunk = class(TMp4ContainerChunk)
  public
    class function GetClassChunkName: TChunkName; override;
  public
    constructor Create; override;
  end;

  TFileMp4 = class(TChunkedFile)
  private
    FFileType: TMp4FileTypeChunk;
    procedure ReadUnknownChunk(Stream: TStream; ChunkName: TChunkName;
      ChunkSize: Cardinal);
  public
    constructor Create; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    class function GetClassChunkName: TChunkName; override;
    class function CanLoad(Stream: TStream): Boolean;
  end;

implementation

uses
  DateUtils, CfeUtils;

{ TMp4DefinedChunk }

constructor TMp4DefinedChunk.Create;
begin
  inherited;

  FChunkFlags := [cfSizeFirst, cfReversedByteOrder, cfIncludeChunkInSize];
end;


{ TMp4UnknownChunk }

constructor TMp4UnknownChunk.Create;
begin
  inherited;

  FChunkFlags := [cfSizeFirst, cfReversedByteOrder, cfIncludeChunkInSize];
end;


{ TMp4FixedDefinedChunk }

constructor TMp4FixedDefinedChunk.Create;
begin
  inherited;

  FChunkFlags := [cfSizeFirst, cfReversedByteOrder, cfIncludeChunkInSize];
end;


{ TMp4ContainerChunk }

constructor TMp4ContainerChunk.Create;
begin
  inherited;

  FChunkFlags := [cfSizeFirst, cfReversedByteOrder, cfIncludeChunkInSize];
end;


{ TMp4TextChunk }

constructor TMp4TextChunk.Create;
begin
  inherited;

  FChunkFlags := [cfSizeFirst, cfReversedByteOrder, cfIncludeChunkInSize];
end;


{ TMp4FreeChunk }

class function TMp4FreeChunk.GetClassChunkName: TChunkName;
begin
  Result := 'free';
end;


{ TMp4MovieDataChunk }

constructor TMp4MovieDataChunk.Create;
begin
  inherited;
  FStream := TMemoryStream.Create;
end;

destructor TMp4MovieDataChunk.Destroy;
begin
  FreeAndNil(FStream);
  inherited;
end;

procedure TMp4MovieDataChunk.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TMp4MovieDataChunk then
  begin
    FStream.Position := 0;
    TMp4MovieDataChunk(Dest).FStream.Position := 0;
    TMp4MovieDataChunk(Dest).FStream.CopyFrom(FStream, FStream.Size);
  end;
end;

function TMp4MovieDataChunk.GetChunkSize: Cardinal;
begin
  FChunkSize := FStream.Size;
  Result := inherited GetChunkSize;
end;

class function TMp4MovieDataChunk.GetClassChunkName: TChunkName;
begin
  Result := 'mdat';
end;

procedure TMp4MovieDataChunk.LoadFromStream(Stream: TStream);
begin
  inherited;
  FStream.Position := 0;
  FStream.CopyFrom(Stream, FChunkSize);
  FStream.Position := 0;

  // eventually skip padded zeroes
  if cfPadSize in ChunkFlags then
    Stream.Position := Stream.Position + CalculateZeroPad;
end;

procedure TMp4MovieDataChunk.SaveToStream(Stream: TStream);
begin
  FChunkSize := FStream.Size;
  inherited;
  FStream.Position := 0;
  Stream.CopyFrom(FStream, FStream.Size);

  // eventually skip padded zeroes
  if (cfPadSize in ChunkFlags) then
    Stream.Position := Stream.Position + CalculateZeroPad;
end;


{ TMp4FileTypeChunk }

class function TMp4FileTypeChunk.GetClassChunkName: TChunkName;
begin
  Result := 'ftyp';
end;

function TMp4FileTypeChunk.GetCompatibleBrandsAsString: String;
var
  Index: Integer;
begin
  if Length(FCompatibleBrands) = 0 then
    Result := '';

  Result := FCompatibleBrands[0];

  for Index := 1 to High(FCompatibleBrands) do
    Result := Result + ', ' + FCompatibleBrands[Index];
end;

procedure TMp4FileTypeChunk.LoadFromStream(Stream: TStream);
var
  Index: Integer;
  CompatibleBrandCount: Integer;
begin
  inherited;

  Stream.Read(FMajorBrand, 4);
  Stream.Read(FMinorVersion, 4);
  CompatibleBrandCount := (FChunkSize - 8) div SizeOf(Integer);
  SetLength(FCompatibleBrands, CompatibleBrandCount);
  for Index := 0 to CompatibleBrandCount - 1 do
    Stream.Read(FCompatibleBrands[Index], 4);
end;

procedure TMp4FileTypeChunk.SaveToStream(Stream: TStream);
var
  Index: Integer;
begin
  inherited;

  Stream.Write(FMajorBrand, 4);
  Stream.Write(FMinorVersion, 4);
  for Index := 0 to Length(FCompatibleBrands) - 1 do
    Stream.Write(FCompatibleBrands[Index], 4);
end;


{ TMp4MovieHeaderChunk }

constructor TMp4MovieHeaderChunk.Create;
begin
  inherited;

  FVersion := 0;
  FFlags[0] := 0;
  FFlags[1] := 0;
  FFlags[2] := 0;
  FCreationTime := SecondsBetween(Now, EncodeDateTime(1904, 1, 1, 0, 0, 0, 0));
  FModificationTime := SecondsBetween(Now, EncodeDateTime(1904, 1, 1, 0, 0, 0, 0));
  FTimeScale := $3E8;
  FDuration := $57BC0;
  FPreferredRate := $10000;
  FPreferredVolume := $100;
  FillChar(FReserved[0], 10, 0);
  FMatrixStructure[0] := $100;
  FMatrixStructure[1] := 0;
  FMatrixStructure[2] := 0;
  FMatrixStructure[3] := 0;
  FMatrixStructure[4] := $100;
  FMatrixStructure[5] := 0;
  FMatrixStructure[6] := 0;
  FMatrixStructure[7] := 0;
  FMatrixStructure[8] := $40;
  FPreviewTime := 0;
  FPreviewDuration := 0;
  FPosterTime := 0;
  FSelectionTime := 0;
  FSelectionDuration := 0;
  FCurrentTime := 0;
  FNextTrackId := $2000000;
end;

class function TMp4MovieHeaderChunk.GetClassChunkName: TChunkName;
begin
  Result := 'mvhd';
end;

function TMp4MovieHeaderChunk.GetCreationTime: TDateTime;
begin
  Result := IncSecond(EncodeDateTime(1904, 1, 1, 0, 0, 0, 0), FCreationTime);
end;

function TMp4MovieHeaderChunk.GetDuration: Single;
begin
  Result :=  FDuration / TimeScale;
end;

function TMp4MovieHeaderChunk.GetModificationTime: TDateTime;
begin
  Result := IncSecond(EncodeDateTime(1904, 1, 1, 0, 0, 0, 0), FModificationTime);
end;

function TMp4MovieHeaderChunk.GetPreferredRate: Single;
begin
  Result := FPreferredRate / $10000;
end;

function TMp4MovieHeaderChunk.GetPreferredVolume: Single;
begin
  Result := FPreferredVolume / $100;
end;

function TMp4MovieHeaderChunk.GetTimeScale: Single;
begin
  Result := FTimeScale;
end;

procedure TMp4MovieHeaderChunk.LoadFromStream(Stream: TStream);
var
  Index: Integer;
begin
  inherited;

  Stream.Read(FVersion, 1);
  Stream.Read(FFlags[0], 3);

  FCreationTime := ReadSwappedCardinal(Stream);
  FModificationTime := ReadSwappedCardinal(Stream);
  FTimeScale := ReadSwappedCardinal(Stream);
  FDuration := ReadSwappedCardinal(Stream);
  FPreferredRate := ReadSwappedCardinal(Stream);
  FPreferredVolume := ReadSwappedWord(Stream);

  // read reserved
  Stream.Read(FReserved[0], 10);

  for Index := Low(FMatrixStructure) to High(FMatrixStructure) do
    FMatrixStructure[Index] := ReadSwappedCardinal(Stream);
  FPreviewTime := ReadSwappedCardinal(Stream);
  FPreviewDuration := ReadSwappedCardinal(Stream);
  FPosterTime := ReadSwappedCardinal(Stream);
  FSelectionTime := ReadSwappedCardinal(Stream);
  FSelectionDuration := ReadSwappedCardinal(Stream);
  FCurrentTime := ReadSwappedCardinal(Stream);
  FNextTrackId := ReadSwappedCardinal(Stream);
end;

procedure TMp4MovieHeaderChunk.SaveToStream(Stream: TStream);
var
  Index: Integer;
begin
  inherited;

  Stream.Write(FVersion, 1);
  Stream.Write(FFlags[0], 3);

  WriteSwappedCardinal(Stream, FCreationTime);
  WriteSwappedCardinal(Stream, FModificationTime);
  WriteSwappedCardinal(Stream, FTimeScale);
  WriteSwappedCardinal(Stream, FDuration);
  WriteSwappedCardinal(Stream, FPreferredRate);
  WriteSwappedWord(Stream, FPreferredVolume);

  // Write reserved
  Stream.Write(FReserved[0], 10);

  for Index := Low(FMatrixStructure) to High(FMatrixStructure) do
    WriteSwappedCardinal(Stream, FMatrixStructure[Index]);
  WriteSwappedCardinal(Stream, FPreviewTime);
  WriteSwappedCardinal(Stream, FPreviewDuration);
  WriteSwappedCardinal(Stream, FPosterTime);
  WriteSwappedCardinal(Stream, FSelectionTime);
  WriteSwappedCardinal(Stream, FSelectionDuration);
  WriteSwappedCardinal(Stream, FCurrentTime);
  WriteSwappedCardinal(Stream, FNextTrackId);
end;


{ TMp4TrackHeaderChunk }

class function TMp4TrackHeaderChunk.GetClassChunkName: TChunkName;
begin
  Result := 'tkhd';
end;

function TMp4TrackHeaderChunk.GetCreationTime: TDateTime;
begin
  Result := IncSecond(EncodeDateTime(1904, 1, 1, 0, 0, 0, 0), FCreationTime);
end;

function TMp4TrackHeaderChunk.GetModificationTime: TDateTime;
begin
  Result := IncSecond(EncodeDateTime(1904, 1, 1, 0, 0, 0, 0), FModificationTime);
end;

procedure TMp4TrackHeaderChunk.LoadFromStream(Stream: TStream);
var
  Index: Integer;
begin
  inherited;

  Stream.Read(FVersion, 1);
  Stream.Read(FFlags[0], 3);

  FCreationTime := ReadSwappedCardinal(Stream);
  FModificationTime := ReadSwappedCardinal(Stream);

  FTrackID := ReadSwappedCardinal(Stream);
  FReservedA := ReadSwappedCardinal(Stream);
  FDuration := ReadSwappedCardinal(Stream);
  Stream.Read(FReservedB, 8);
  FLayer := ReadSwappedWord(Stream);
  FAlternateGroup := ReadSwappedWord(Stream);
  FVolume := ReadSwappedWord(Stream);
  FReservedC := ReadSwappedWord(Stream);
  for Index := Low(FMatrixStructure) to High(FMatrixStructure) do
    FMatrixStructure[Index] := ReadSwappedCardinal(Stream);
  FTrackWidth := ReadSwappedCardinal(Stream);
  FTrackHeight := ReadSwappedCardinal(Stream);
end;

procedure TMp4TrackHeaderChunk.SaveToStream(Stream: TStream);
var
  Index: Integer;
begin
  inherited;

  Stream.Write(FVersion, 1);
  Stream.Write(FFlags[0], 3);

  WriteSwappedCardinal(Stream, FCreationTime);
  WriteSwappedCardinal(Stream, FModificationTime);

  WriteSwappedCardinal(Stream, FTrackID);
  WriteSwappedCardinal(Stream, FReservedA);
  WriteSwappedCardinal(Stream, FDuration);
  Stream.Write(FReservedB, 8);
  WriteSwappedWord(Stream, FLayer);
  WriteSwappedWord(Stream, FAlternateGroup);
  WriteSwappedWord(Stream, FVolume);
  WriteSwappedWord(Stream, FReservedC);
  for Index := Low(FMatrixStructure) to High(FMatrixStructure) do
    WriteSwappedCardinal(Stream, FMatrixStructure[Index]);
  WriteSwappedCardinal(Stream, FTrackWidth);
  WriteSwappedCardinal(Stream, FTrackHeight);
end;


{ TMp4EditListEntry }

procedure TMp4EditListEntry.LoadFromStream(Stream: TStream);
begin
  FTrackDuration := ReadSwappedCardinal(Stream);
  FMediaTime := ReadSwappedCardinal(Stream);
  FMediaRate := ReadSwappedCardinal(Stream);
end;

procedure TMp4EditListEntry.SaveToStream(Stream: TStream);
begin
  WriteSwappedCardinal(Stream, FTrackDuration);
  WriteSwappedCardinal(Stream, FMediaTime);
  WriteSwappedCardinal(Stream, FMediaRate);
end;


{ TMp4EditListEntryList }

function TMp4EditListEntryList.Add(AChunk: TMp4EditListEntry): Integer;
begin
  Result := inherited Add(TObject(AChunk));
end;

function TMp4EditListEntryList.Extract(Item: TMp4EditListEntry): TMp4EditListEntry;
begin
  Result := TMp4EditListEntry(inherited Extract(TObject(Item)));
end;

function TMp4EditListEntryList.First: TMp4EditListEntry;
begin
  Result := TMp4EditListEntry(inherited First);
end;

function TMp4EditListEntryList.GetItem(Index: Integer): TMp4EditListEntry;
begin
  Result := TMp4EditListEntry(inherited GetItem(Index));
end;

function TMp4EditListEntryList.IndexOf(AChunk: TMp4EditListEntry): Integer;
begin
  Result := inherited IndexOf(TObject(AChunk));
end;

procedure TMp4EditListEntryList.Insert(Index: Integer; AChunk: TMp4EditListEntry);
begin
  inherited Insert(Index, TObject(AChunk));
end;

function TMp4EditListEntryList.Last: TMp4EditListEntry;
begin
  Result := TMp4EditListEntry(inherited Last);
end;

function TMp4EditListEntryList.Remove(AChunk: TMp4EditListEntry): Integer;
begin
  Result := inherited Remove(TObject(AChunk));
end;

procedure TMp4EditListEntryList.SetItem(Index: Integer;
  AChunk: TMp4EditListEntry);
begin
  inherited SetItem(Index, TObject(AChunk));
end;


{ TMp4EditListChunk }

constructor TMp4EditListChunk.Create;
begin
  inherited;

  FList := TMp4EditListEntryList.Create;
end;

destructor TMp4EditListChunk.Destroy;
begin
  FList.Free;

  inherited;
end;

class function TMp4EditListChunk.GetClassChunkName: TChunkName;
begin
  Result := 'elst';
end;

procedure TMp4EditListChunk.LoadFromStream(Stream: TStream);
var
  Index: Integer;
  ListEntry: TMp4EditListEntry;
begin
  inherited;

  Stream.Read(FVersion, 1);
  Stream.Read(FFlags[0], 3);

  FNumberOfEntries := ReadSwappedCardinal(Stream);

  for Index := 0 to FNumberOfEntries - 1 do
  begin
    ListEntry := TMp4EditListEntry.Create;
    ListEntry.LoadFromStream(Stream);
    FList.Add(ListEntry);
  end;
end;

procedure TMp4EditListChunk.SaveToStream(Stream: TStream);
var
  Index: Integer;
begin
  inherited;

  Stream.Write(FVersion, 1);
  Stream.Write(FFlags[0], 3);

  FNumberOfEntries := FList.Count;
  WriteSwappedCardinal(Stream, FNumberOfEntries);

  for Index := 0 to FList.Count - 1 do
    FList[Index].SaveToStream(Stream);
end;


{ TMp4EditChunk }

constructor TMp4EditChunk.Create;
begin  inherited;

  RegisterChunkClasses([TMp4EditListChunk]);
end;

class function TMp4EditChunk.GetClassChunkName: TChunkName;
begin
  Result := 'edts';
end;


{ TMp4MediaHeaderChunk }

class function TMp4MediaHeaderChunk.GetClassChunkName: TChunkName;
begin
  Result := 'mdhd';
end;

function TMp4MediaHeaderChunk.GetCreationTime: TDateTime;
begin
  Result := IncSecond(EncodeDateTime(1904, 1, 1, 0, 0, 0, 0), FCreationTime);
end;

function TMp4MediaHeaderChunk.GetModificationTime: TDateTime;
begin
  Result := IncSecond(EncodeDateTime(1904, 1, 1, 0, 0, 0, 0), FModificationTime);
end;

function TMp4MediaHeaderChunk.GetDuration: Single;
begin
  Result :=  FDuration / FTimeScale;
end;

function TMp4MediaHeaderChunk.GetTimeScale: Single;
begin
  Result :=  FTimeScale;
end;


procedure TMp4MediaHeaderChunk.LoadFromStream(Stream: TStream);
begin
  inherited;

  Stream.Read(FVersion, 1);
  Stream.Read(FFlags, 3);
  FCreationTime := ReadSwappedCardinal(Stream);
  FModificationTime := ReadSwappedCardinal(Stream);
  FTimeScale := ReadSwappedCardinal(Stream);
  FDuration := ReadSwappedCardinal(Stream);
  FLanguage := ReadSwappedWord(Stream);
  FQuality := ReadSwappedWord(Stream);
end;

procedure TMp4MediaHeaderChunk.SaveToStream(Stream: TStream);
begin
  inherited;

  Stream.Write(FVersion, 1);
  Stream.Write(FFlags, 3);
  WriteSwappedCardinal(Stream, FCreationTime);
  WriteSwappedCardinal(Stream, FModificationTime);
  WriteSwappedCardinal(Stream, FTimeScale);
  WriteSwappedCardinal(Stream, FDuration);
  WriteSwappedWord(Stream, FLanguage);
  WriteSwappedWord(Stream, FQuality);
end;


{ TMp4HandlerReferenceChunk }

class function TMp4HandlerReferenceChunk.GetClassChunkName: TChunkName;
begin
  Result := 'hdlr';
end;

procedure TMp4HandlerReferenceChunk.LoadFromStream(Stream: TStream);
begin
  inherited;

  Stream.Read(FVersion, 1);
  Stream.Read(FFlags, 3);

  Stream.Read(FComponentType, 4);
  Stream.Read(FComponentSubType, 4);
  Stream.Read(FComponentManufacturer, 4);
  Stream.Read(FComponentFlags, 4);
  Stream.Read(FComponentFlagMask, 4);
  SetLength(FComponentName, FChunkSize - 24);
  Stream.Read(FComponentName[1], FChunkSize - 24);
end;

procedure TMp4HandlerReferenceChunk.SaveToStream(Stream: TStream);
begin
  inherited;

  Stream.Write(FVersion, 1);
  Stream.Write(FFlags, 3);

  Stream.Write(FComponentType, 4);
  Stream.Write(FComponentSubType, 4);
  Stream.Write(FComponentManufacturer, 4);
  Stream.Write(FComponentFlags, 4);
  Stream.Write(FComponentFlagMask, 4);
  Stream.Write(FComponentName[1], Length(FComponentName));
end;


{ TMp4VideoMediaInformationChunk }

class function TMp4VideoMediaInformationChunk.GetClassChunkName: TChunkName;
begin
  Result := 'vmhd'
end;

procedure TMp4VideoMediaInformationChunk.LoadFromStream(Stream: TStream);
begin
  inherited;

  Stream.Read(FVersion, 1);
  Stream.Read(FFlags, 3);
  FGraphicsMode := ReadSwappedWord(Stream);
  Stream.Read(FOpColor, 3);
end;

procedure TMp4VideoMediaInformationChunk.SaveToStream(Stream: TStream);
begin
  inherited;

  Stream.Write(FVersion, 1);
  Stream.Write(FFlags, 3);
  WriteSwappedWord(Stream, FGraphicsMode);
  Stream.Write(FOpColor, 3);
end;


{ TMp4SoundMediaInformationChunk }

class function TMp4SoundMediaInformationChunk.GetClassChunkName: TChunkName;
begin
  Result := 'smhd'
end;

procedure TMp4SoundMediaInformationChunk.LoadFromStream(Stream: TStream);
begin
  inherited;

  Stream.Read(FVersion, 1);
  Stream.Read(FFlags, 3);

  FBalance := ReadSwappedWord(Stream);
  FReserved := ReadSwappedWord(Stream);
end;

procedure TMp4SoundMediaInformationChunk.SaveToStream(Stream: TStream);
begin
  inherited;

  Stream.Write(FVersion, 1);
  Stream.Write(FFlags, 3);

  WriteSwappedWord(Stream, FBalance);
  WriteSwappedWord(Stream, FReserved);
end;


{ TMp4MediaInformationChunk }

constructor TMp4MediaInformationChunk.Create;
begin
  inherited;

  RegisterChunkClasses([TMp4VideoMediaInformationChunk,
    TMp4SoundMediaInformationChunk, TMp4BaseMediaInformationChunk,
    TMp4HandlerReferenceChunk, TMp4DataInformationChunk, TMp4SampleTableChunk]);
end;

class function TMp4MediaInformationChunk.GetClassChunkName: TChunkName;
begin
  Result := 'minf';
end;


{ TMp4BaseMediaInformationChunk }

class function TMp4BaseMediaInformationChunk.GetClassChunkName: TChunkName;
begin
  Result := 'gmin';
end;

procedure TMp4BaseMediaInformationChunk.LoadFromStream(Stream: TStream);
begin
  inherited;

  Stream.Read(FVersion, 1);
  Stream.Read(FFlags, 3);
  FGraphicsMode := ReadSwappedWord(Stream);
  Stream.Read(FOpColor, 3);
  FBalance := ReadSwappedWord(Stream);
  FReserved := ReadSwappedWord(Stream);
end;

procedure TMp4BaseMediaInformationChunk.SaveToStream(Stream: TStream);
begin
  inherited;

  Stream.Write(FVersion, 1);
  Stream.Write(FFlags, 3);

  WriteSwappedWord(Stream, FGraphicsMode);
  Stream.Write(FOpColor, 3);

  WriteSwappedWord(Stream, FBalance);
  WriteSwappedWord(Stream, FReserved);
end;


{ TMp4DataReferenceChunk }

class function TMp4DataReferenceChunk.GetClassChunkName: TChunkName;
begin
  Result := 'dref';
end;

procedure TMp4DataReferenceChunk.LoadFromStream(Stream: TStream);
var
  Index: Integer;
  SubChunkName: TChunkName;
  SubChunkSize: Cardinal;
begin
  // read chunk size
  Stream.Read(FChunkSize, 4);
  Flip32(FChunkSize);

  // read chunk name
  Stream.Read(FChunkName, 4);

  // read version
  Stream.Read(FVersion, 1);

  // read flags
  Stream.Read(FFlags, 3);

  // read number of entries
  FNumberOfEntries := ReadSwappedCardinal(Stream);

  for Index := 0 to FNumberOfEntries - 1 do
  begin
    // read chunk size
    SubChunkSize := ReadSwappedCardinal(Stream);

    // read chunk name
    Stream.Read(SubChunkName, 4);

    Stream.Position := Stream.Position - 8;
    ConvertStreamToChunk(GetChunkClass(SubChunkName), Stream);
  end;

  // eventually skip padded zeroes
  if cfPadSize in ChunkFlags then
    Stream.Position := Stream.Position + CalculateZeroPad;
end;

procedure TMp4DataReferenceChunk.SaveToStream(Stream: TStream);
var
  Value: Cardinal;
  i: Integer;
begin
  // write chunk size
  FChunkSize := GetChunkSize;
  Value := FChunkSize;
  Flip32(Value);
  Stream.Write(Value, 4);

  // write chunk name
  Stream.Write(FChunkName, 4);

  // write version
  Stream.Write(FVersion, 1);

  // write flags
  Stream.Write(FFlags, 3);

  // write number of entries
  WriteSwappedCardinal(Stream, FNumberOfEntries);

  for i := 0 to FChunkList.Count - 1 do
    FChunkList[i].SaveToStream(Stream);

  // insert pad byte if necessary
  if cfPadSize in ChunkFlags then
    Stream.Write(CZeroPad, CalculateZeroPad);
end;


{ TMp4DataInformationChunk }

constructor TMp4DataInformationChunk.Create;
begin
  inherited;

  RegisterChunkClass(TMp4DataReferenceChunk);
end;

class function TMp4DataInformationChunk.GetClassChunkName: TChunkName;
begin
  Result := 'dinf';
end;


{ TMp4ElementaryStreamDescriptorChunk }

constructor TMp4ElementaryStreamDescriptorChunk.Create;
begin
  inherited;

  FBitStream := TMemoryStream.Create;
end;

destructor TMp4ElementaryStreamDescriptorChunk.Destroy;
begin
  FBitStream.Free;

  inherited;
end;

class function TMp4ElementaryStreamDescriptorChunk.GetClassChunkName: TChunkName;
begin
  Result := 'esds';
end;

procedure TMp4ElementaryStreamDescriptorChunk.LoadFromStream(Stream: TStream);
begin
  inherited;

  FBitStream.CopyFrom(Stream, ChunkSize);
end;

procedure TMp4ElementaryStreamDescriptorChunk.SaveToStream(Stream: TStream);
begin
  inherited;

  Stream.CopyFrom(FBitStream, ChunkSize);
end;


{ TMp4Mp4aChunk }

constructor TMp4Mp4aChunk.Create;
begin
  inherited;

end;

class function TMp4Mp4aChunk.GetClassChunkName: TChunkName;
begin
  Result := 'mp4a';
end;

procedure TMp4Mp4aChunk.LoadFromStream(Stream: TStream);
begin
  Stream.Read(FUnknownA, 12);
  FDataReferenceIndex := ReadSwappedCardinal(Stream);
  Stream.Read(FUnknownB, 8);
  FChannelCount := ReadSwappedWord(Stream);
  FSampleSize := ReadSwappedWord(Stream);
  Stream.Read(FUnknownC, 2);
  FSampleRate := ReadSwappedCardinal(Stream);
  Stream.Read(FUnknownD, 2);
end;

procedure TMp4Mp4aChunk.SaveToStream(Stream: TStream);
begin
  Stream.Write(FUnknownA, 12);
  WriteSwappedCardinal(Stream, FDataReferenceIndex);
  Stream.Write(FUnknownB, 8);
  WriteSwappedWord(Stream, FChannelCount);
  WriteSwappedWord(Stream, FSampleSize);
  Stream.Write(FUnknownC, 2);
  WriteSwappedCardinal(Stream, FSampleRate);
  Stream.Write(FUnknownD, 2);
end;


{ TMp4SampleDescriptionChunk }

constructor TMp4SampleDescriptionChunk.Create;
begin
  inherited;

  RegisterChunkClass(TMp4Mp4aChunk);
end;

class function TMp4SampleDescriptionChunk.GetClassChunkName: TChunkName;
begin
  Result := 'stsd';
end;

procedure TMp4SampleDescriptionChunk.LoadFromStream(Stream: TStream);
var
  Index: Integer;
  SubChunkName: TChunkName;
  SubChunkSize: Cardinal;
begin
  // read chunk size
  Stream.Read(FChunkSize, 4);
  Flip32(FChunkSize);

  // read chunk name
  Stream.Read(FChunkName, 4);

  Stream.Read(FVersion, 1);
  Stream.Read(FFlags, 3);

  FNumberOfEntries := ReadSwappedCardinal(Stream);
  for Index := 0 to FNumberOfEntries - 1 do
  begin
    // read chunk size
    SubChunkSize := ReadSwappedCardinal(Stream);

    // read chunk name
    Stream.Read(SubChunkName, 4);

    Stream.Position := Stream.Position - 8;
    ConvertStreamToChunk(GetChunkClass(SubChunkName), Stream);
  end;

  // eventually skip padded zeroes
  if cfPadSize in ChunkFlags then
    Stream.Position := Stream.Position + CalculateZeroPad;
end;

procedure TMp4SampleDescriptionChunk.SaveToStream(Stream: TStream);
var
  Value: Cardinal;
  i: Integer;
begin
  // write chunk size
  FChunkSize := GetChunkSize;
  WriteSwappedCardinal(Stream, FChunkSize);

  // write chunk name
  Stream.Write(FChunkName, 4);

  // write number of entries
  WriteSwappedCardinal(Stream, FNumberOfEntries);

  for i := 0 to FChunkList.Count - 1 do
    FChunkList[i].SaveToStream(Stream);

  // insert pad byte if necessary
  if cfPadSize in ChunkFlags then
    Stream.Write(CZeroPad, CalculateZeroPad);
end;


{ TMp4SampleTableEntry }

procedure TMp4SampleTableEntry.LoadFromStream(Stream: TStream);
begin
  FSampleCount := ReadSwappedCardinal(Stream);
  FSampleDuration := ReadSwappedCardinal(Stream);
end;

procedure TMp4SampleTableEntry.SaveToStream(Stream: TStream);
begin
  WriteSwappedCardinal(Stream, FSampleCount);
  WriteSwappedCardinal(Stream, FSampleDuration);
end;


{ TMp4SampleTableEntryList }

function TMp4SampleTableEntryList.Add(AChunk: TMp4SampleTableEntry): Integer;
begin
  Result := inherited Add(TObject(AChunk));
end;

function TMp4SampleTableEntryList.Extract(
  Item: TMp4SampleTableEntry): TMp4SampleTableEntry;
begin
  Result := TMp4SampleTableEntry(inherited Extract(TObject(Item)));
end;

function TMp4SampleTableEntryList.First: TMp4SampleTableEntry;
begin
  Result := TMp4SampleTableEntry(inherited First);
end;

function TMp4SampleTableEntryList.GetItem(Index: Integer): TMp4SampleTableEntry;
begin
  Result := TMp4SampleTableEntry(inherited GetItem(Index));
end;

function TMp4SampleTableEntryList.IndexOf(AChunk: TMp4SampleTableEntry): Integer;
begin
  Result := inherited IndexOf(TObject(AChunk));
end;

procedure TMp4SampleTableEntryList.Insert(Index: Integer;
  AChunk: TMp4SampleTableEntry);
begin
  inherited Insert(Index, TObject(AChunk));
end;

function TMp4SampleTableEntryList.Last: TMp4SampleTableEntry;
begin
  Result := TMp4SampleTableEntry(inherited Last);
end;

function TMp4SampleTableEntryList.Remove(AChunk: TMp4SampleTableEntry): Integer;
begin
  Result := inherited Remove(TObject(AChunk));
end;

procedure TMp4SampleTableEntryList.SetItem(Index: Integer;
  AChunk: TMp4SampleTableEntry);
begin
  inherited SetItem(Index, TObject(AChunk));
end;


{ TMp4TimeToSampleChunk }

constructor TMp4TimeToSampleChunk.Create;
begin
  inherited;

  FSampleTable := TMp4SampleTableEntryList.Create;
end;

destructor TMp4TimeToSampleChunk.Destroy;
begin
  FSampleTable.Free;

  inherited;
end;

class function TMp4TimeToSampleChunk.GetClassChunkName: TChunkName;
begin
  Result := 'stts';
end;

procedure TMp4TimeToSampleChunk.LoadFromStream(Stream: TStream);
var
  Index: Integer;
  ListEntry: TMp4SampleTableEntry;
begin
  inherited;

  Stream.Read(FVersion, 1);
  Stream.Read(FFlags, 3);

  FNumberOfEntries := ReadSwappedCardinal(Stream);

  for Index := 0 to FNumberOfEntries - 1 do
  begin
    ListEntry := TMp4SampleTableEntry.Create;
    ListEntry.LoadFromStream(Stream);
    FSampleTable.Add(ListEntry);
  end;
end;

procedure TMp4TimeToSampleChunk.SaveToStream(Stream: TStream);
var
  Index: Integer;
begin
  inherited;

  Stream.Write(FVersion, 1);
  Stream.Write(FFlags, 3);

  WriteSwappedCardinal(Stream, FNumberOfEntries);

  for Index := 0 to FSampleTable.Count - 1 do
    FSampleTable[Index].SaveToStream(Stream);
end;


{ TMp4SampleToChunkEntry }

procedure TMp4SampleToChunkEntry.LoadFromStream(Stream: TStream);
begin
  FFirstChunk := ReadSwappedCardinal(Stream);
  FSamplesPerChunk := ReadSwappedCardinal(Stream);
  FSampleDescription := ReadSwappedCardinal(Stream);
end;

procedure TMp4SampleToChunkEntry.SaveToStream(Stream: TStream);
begin
  WriteSwappedCardinal(Stream, FFirstChunk);
  WriteSwappedCardinal(Stream, FSamplesPerChunk);
  WriteSwappedCardinal(Stream, FSampleDescription);
end;


{ TMp4SampleToChunkEntryList }

function TMp4SampleToChunkEntryList.Add(AChunk: TMp4SampleToChunkEntry): Integer;
begin
  Result := inherited Add(TObject(AChunk));
end;

function TMp4SampleToChunkEntryList.Extract(
  Item: TMp4SampleToChunkEntry): TMp4SampleToChunkEntry;
begin
  Result := TMp4SampleToChunkEntry(inherited Extract(TObject(Item)));
end;

function TMp4SampleToChunkEntryList.First: TMp4SampleToChunkEntry;
begin
  Result := TMp4SampleToChunkEntry(inherited First);
end;

function TMp4SampleToChunkEntryList.GetItem(Index: Integer): TMp4SampleToChunkEntry;
begin
  Result := TMp4SampleToChunkEntry(inherited GetItem(Index));
end;

function TMp4SampleToChunkEntryList.IndexOf(AChunk: TMp4SampleToChunkEntry): Integer;
begin
  Result := inherited IndexOf(TObject(AChunk));
end;

procedure TMp4SampleToChunkEntryList.Insert(Index: Integer;
  AChunk: TMp4SampleToChunkEntry);
begin
  inherited Insert(Index, TObject(AChunk));
end;

function TMp4SampleToChunkEntryList.Last: TMp4SampleToChunkEntry;
begin
  Result := TMp4SampleToChunkEntry(inherited Last);
end;

function TMp4SampleToChunkEntryList.Remove(AChunk: TMp4SampleToChunkEntry): Integer;
begin
  Result := inherited Remove(TObject(AChunk));
end;

procedure TMp4SampleToChunkEntryList.SetItem(Index: Integer;
  AChunk: TMp4SampleToChunkEntry);
begin
  inherited SetItem(Index, TObject(AChunk));
end;


{ TMp4SampleToChunkChunk }

constructor TMp4SampleToChunkChunk.Create;
begin
  inherited;

  FSampleToChunkTable := TMp4SampleToChunkEntryList.Create;
end;

destructor TMp4SampleToChunkChunk.Destroy;
begin
  FSampleToChunkTable.Free;

  inherited;
end;

class function TMp4SampleToChunkChunk.GetClassChunkName: TChunkName;
begin
  Result := 'stsc';
end;

procedure TMp4SampleToChunkChunk.LoadFromStream(Stream: TStream);
var
  Index: Integer;
  ListEntry: TMp4SampleToChunkEntry;
begin
  inherited;

  Stream.Read(FVersion, 1);
  Stream.Read(FFlags, 3);

  FNumberOfEntries := ReadSwappedCardinal(Stream);

  for Index := 0 to FNumberOfEntries - 1 do
  begin
    ListEntry := TMp4SampleToChunkEntry.Create;
    ListEntry.LoadFromStream(Stream);
    FSampleToChunkTable.Add(ListEntry);
  end;
end;

procedure TMp4SampleToChunkChunk.SaveToStream(Stream: TStream);
var
  Index: Integer;
begin
  inherited;

  Stream.Write(FVersion, 1);
  Stream.Write(FFlags, 3);

  FNumberOfEntries := FSampleToChunkTable.Count;
  WriteSwappedCardinal(Stream, FNumberOfEntries);

  for Index := 0 to FSampleToChunkTable.Count - 1 do
    FSampleToChunkTable[Index].SaveToStream(Stream);
end;


{ TMp4SampleSizeChunk }

constructor TMp4SampleSizeChunk.Create;
begin
  inherited;
end;

class function TMp4SampleSizeChunk.GetClassChunkName: TChunkName;
begin
  Result := 'stsz';
end;

procedure TMp4SampleSizeChunk.LoadFromStream(Stream: TStream);
var
  Index: Integer;
begin
  inherited;

  Stream.Read(FVersion, 1);
  Stream.Read(FFlags, 3);

  FSampleSize := ReadSwappedCardinal(Stream);

  FNumberOfEntries := ReadSwappedCardinal(Stream);

  SetLength(FSampleSizes, FNumberOfEntries);
  for Index := 0 to FNumberOfEntries - 1 do
    FSampleSizes[Index] := ReadSwappedCardinal(Stream);
end;

procedure TMp4SampleSizeChunk.SaveToStream(Stream: TStream);
var
  Index: Integer;
begin
  inherited;

  Stream.Write(FVersion, 1);
  Stream.Write(FFlags, 3);

  WriteSwappedCardinal(Stream, FSampleSize);

  FNumberOfEntries := Length(FSampleSizes);
  WriteSwappedCardinal(Stream, FNumberOfEntries);

  for Index := 0 to Length(FSampleSizes) - 1 do
    WriteSwappedCardinal(Stream, FSampleSizes[Index]);
end;


{ TMp4ChunkOffsetChunk }

constructor TMp4ChunkOffsetChunk.Create;
begin
  inherited;
end;

class function TMp4ChunkOffsetChunk.GetClassChunkName: TChunkName;
begin
  Result := 'stco';
end;

procedure TMp4ChunkOffsetChunk.LoadFromStream(Stream: TStream);
var
  Index: Integer;
begin
  inherited;

  Stream.Read(FVersion, 1);
  Stream.Read(FFlags, 3);

  FNumberOfEntries := ReadSwappedCardinal(Stream);

  // TODO: check for 32/64-bit
  SetLength(FChunkOffsetTable, FNumberOfEntries);
  for Index := 0 to FNumberOfEntries - 1 do
    FChunkOffsetTable[Index] := ReadSwappedCardinal(Stream);
end;

procedure TMp4ChunkOffsetChunk.SaveToStream(Stream: TStream);
var
  Index: Integer;
begin
  inherited;

  Stream.Write(FVersion, 1);
  Stream.Write(FFlags, 3);

  FNumberOfEntries := Length(FChunkOffsetTable);
  WriteSwappedCardinal(Stream, FNumberOfEntries);

  // TODO: check for 32/64-bit
  for Index := 0 to Length(FChunkOffsetTable) - 1 do
    WriteSwappedCardinal(Stream, FChunkOffsetTable[Index]);
end;


{ TMp4SampleGroupDescriptionChunk }

constructor TMp4SampleGroupDescriptionChunk.Create;
begin
  inherited;

  FGroupingType := 'roll';
  FDefaultLength := 2;
end;

class function TMp4SampleGroupDescriptionChunk.GetClassChunkName: TChunkName;
begin
  Result := 'sgpd';
end;

procedure TMp4SampleGroupDescriptionChunk.LoadFromStream(Stream: TStream);
var
  Index: Integer;
begin
  inherited;

  Stream.Read(FVersion, 1);
  Stream.Read(FFlags, 3);

  Stream.Read(FGroupingType, 4);
  FDefaultLength := ReadSwappedCardinal(Stream);

  FEntryCount := ReadSwappedCardinal(Stream);

  SetLength(FPayload, 1);
  for Index := 0 to FEntryCount - 1 do
    FPayload[Index] := ReadSwappedWord(Stream);
end;

procedure TMp4SampleGroupDescriptionChunk.SaveToStream(Stream: TStream);
var
  Index: Integer;
begin
  inherited;

  Stream.Write(FVersion, 1);
  Stream.Write(FFlags, 3);

  Stream.Write(FGroupingType, 4);

  FEntryCount := Length(FPayload);
  WriteSwappedCardinal(Stream, FEntryCount);

  for Index := 0 to Length(FPayload) - 1 do
    WriteSwappedWord(Stream, FPayload[Index]);
end;


{ TMp4SampleToGroupEntry }

procedure TMp4SampleToGroupEntry.LoadFromStream(Stream: TStream);
begin
  FSampleCount := ReadSwappedCardinal(Stream);
  FGroupDescriptionIndex := ReadSwappedCardinal(Stream);
end;

procedure TMp4SampleToGroupEntry.SaveToStream(Stream: TStream);
begin
  WriteSwappedCardinal(Stream, FSampleCount);
  WriteSwappedCardinal(Stream, FGroupDescriptionIndex);
end;


{ TMp4SampleToGroupEntryList }

function TMp4SampleToGroupEntryList.Add(AChunk: TMp4SampleToGroupEntry): Integer;
begin
  Result := inherited Add(TObject(AChunk));
end;

function TMp4SampleToGroupEntryList.Extract(
  Item: TMp4SampleToGroupEntry): TMp4SampleToGroupEntry;
begin
  Result := TMp4SampleToGroupEntry(inherited Extract(TObject(Item)));
end;

function TMp4SampleToGroupEntryList.First: TMp4SampleToGroupEntry;
begin
  Result := TMp4SampleToGroupEntry(inherited First);
end;

function TMp4SampleToGroupEntryList.GetItem(Index: Integer): TMp4SampleToGroupEntry;
begin
  Result := TMp4SampleToGroupEntry(inherited GetItem(Index));
end;

function TMp4SampleToGroupEntryList.IndexOf(AChunk: TMp4SampleToGroupEntry): Integer;
begin
  Result := inherited IndexOf(TObject(AChunk));
end;

procedure TMp4SampleToGroupEntryList.Insert(Index: Integer;
  AChunk: TMp4SampleToGroupEntry);
begin
  inherited Insert(Index, TObject(AChunk));
end;

function TMp4SampleToGroupEntryList.Last: TMp4SampleToGroupEntry;
begin
  Result := TMp4SampleToGroupEntry(inherited Last);
end;

function TMp4SampleToGroupEntryList.Remove(AChunk: TMp4SampleToGroupEntry): Integer;
begin
  Result := inherited Remove(TObject(AChunk));
end;

procedure TMp4SampleToGroupEntryList.SetItem(Index: Integer;
  AChunk: TMp4SampleToGroupEntry);
begin
  inherited SetItem(Index, TObject(AChunk));
end;


{ TMp4SampleToGroupChunk }

constructor TMp4SampleToGroupChunk.Create;
begin
  inherited;

  FGroupingType := 'roll';
  FTableData := TMp4SampleToGroupEntryList.Create;
end;

destructor TMp4SampleToGroupChunk.Destroy;
begin
  FTableData.Free;

  inherited;
end;

class function TMp4SampleToGroupChunk.GetClassChunkName: TChunkName;
begin
  Result := 'sbgp';
end;

procedure TMp4SampleToGroupChunk.LoadFromStream(Stream: TStream);
var
  Index: Integer;
  ListEntry: TMp4SampleToGroupEntry;
begin
  inherited;

  Stream.Read(FVersion, 1);
  Stream.Read(FFlags, 3);

  Stream.Read(FGroupingType, 4);

  FEntryCount := ReadSwappedCardinal(Stream);

  for Index := 0 to FEntryCount - 1 do
  begin
    ListEntry := TMp4SampleToGroupEntry.Create;
    ListEntry.LoadFromStream(Stream);
    FTableData.Add(ListEntry);
  end;
end;

procedure TMp4SampleToGroupChunk.SaveToStream(Stream: TStream);
var
  Index: Integer;
begin
  inherited;

  Stream.Write(FVersion, 1);
  Stream.Write(FFlags, 3);

  Stream.Write(FGroupingType, 4);

  FEntryCount := FTableData.Count;
  WriteSwappedCardinal(Stream, FEntryCount);

  for Index := 0 to FTableData.Count - 1 do
    FTableData[Index].SaveToStream(Stream);
end;


{ TMp4SampleTableChunk }

constructor TMp4SampleTableChunk.Create;
begin
  inherited;

  RegisterChunkClasses([TMp4SampleDescriptionChunk, TMp4TimeToSampleChunk,
    TMp4SampleToChunkChunk, TMp4SampleSizeChunk, TMp4ChunkOffsetChunk,
    TMp4SampleGroupDescriptionChunk, TMp4SampleToGroupChunk]);
end;

class function TMp4SampleTableChunk.GetClassChunkName: TChunkName;
begin
  Result := 'stbl';
end;


{ TMp4MediaChunk }

constructor TMp4MediaChunk.Create;
begin
  inherited;

  RegisterChunkClasses([TMp4MediaHeaderChunk, TMp4HandlerReferenceChunk,
    TMp4MediaInformationChunk]);
end;

class function TMp4MediaChunk.GetClassChunkName: TChunkName;
begin
  Result := 'mdia';
end;


{ TMp4TrackChunk }

constructor TMp4TrackChunk.Create;
begin
  inherited;

  RegisterChunkClasses([TMp4TrackHeaderChunk, TMp4EditChunk, TMp4MediaChunk]);
end;

class function TMp4TrackChunk.GetClassChunkName: TChunkName;
begin
  Result := 'trak';
end;


{ TMp4UserDataChunk }

constructor TMp4UserDataChunk.Create;
begin
  inherited;

  RegisterChunkClass(TMp4MetaChunk);
end;

class function TMp4UserDataChunk.GetClassChunkName: TChunkName;
begin
  Result := 'udta';
end;


{ TMp4DataChunk }

class function TMp4DataChunk.GetClassChunkName: TChunkName;
begin
  Result := 'data';
end;

procedure TMp4DataChunk.LoadFromStream(Stream: TStream);
begin
  inherited;

  // read type
  Stream.Read(FType, 4);
  Flip32(FType);

  // read locale
  Stream.Read(FLocale, 4);
  Flip32(FLocale);

  SetLength(FData, FChunkSize - 8);
  Stream.Read(FData[1], FChunkSize - 8);

  if cfPadSize in ChunkFlags then
    Stream.Position := Stream.Position + CalculateZeroPad;
end;

procedure TMp4DataChunk.SaveToStream(Stream: TStream);
begin
  FChunkSize := 8;
  inherited;

  // read type
  Stream.Write(FType, 4);

  // read locale
  Stream.Write(FLocale, 4);

  Stream.Write(FData[1], Length(FData));
end;


{ TMp4CustomItemListChunk }

constructor TMp4CustomItemListChunk.Create;
begin
  inherited;

  RegisterChunkClasses([TMp4DataChunk]);
end;


destructor TMp4CustomItemListChunk.Destroy;
begin

  inherited;
end;

{ TMp4ItemListArtChunk }

class function TMp4ItemListArtChunk.GetClassChunkName: TChunkName;
begin
  Result := #$A9+'ART';
end;


{ TMp4ItemListTooChunk }

class function TMp4ItemListTooChunk.GetClassChunkName: TChunkName;
begin
  Result := #$A9+'too';
end;


{ TMp4ItemListChunk }

constructor TMp4ItemListChunk.Create;
begin
  inherited;

  RegisterChunkClasses([TMp4ItemListArtChunk, TMp4ItemListTooChunk]);
end;

class function TMp4ItemListChunk.GetClassChunkName: TChunkName;
begin
  Result := 'ilst';
end;


{ TMp4MetaChunk }

constructor TMp4MetaChunk.Create;
begin
  inherited;

  RegisterChunkClasses([TMp4HandlerReferenceChunk, TMp4ItemListChunk]);
end;

class function TMp4MetaChunk.GetClassChunkName: TChunkName;
begin
  Result := 'meta';
end;

procedure TMp4MetaChunk.LoadFromStream(Stream: TStream);
var
  ChunkEnd: Cardinal;
  SubChunkName: TChunkName;
begin
  // read chunk size
  Stream.Read(FChunkSize, 4);
  Flip32(FChunkSize);

  // read chunk name
  Stream.Read(FChunkName, 4);

  // read yet unknown value
  Stream.Read(FUnknown, 4);

  with Stream do
  begin
    ChunkEnd := Position + FChunkSize - 12;
    Assert(ChunkEnd <= Stream.Size);
    while Position < ChunkEnd do
    begin
      Position := Position + 4;
      Read(SubChunkName, 4);
      Position := Position - 8;
      ConvertStreamToChunk(GetChunkClass(SubChunkName), Stream);
    end;
    if Position <> ChunkEnd then
      Position := ChunkEnd;

    // eventually skip padded zeroes
    if cfPadSize in ChunkFlags then
      Position := Position + CalculateZeroPad;
  end;

  // eventually skip padded zeroes
  if cfPadSize in ChunkFlags then
    Stream.Position := Stream.Position + CalculateZeroPad;
end;

procedure TMp4MetaChunk.SaveToStream(Stream: TStream);
var
  Value: Cardinal;
  i: Integer;
begin
  // write chunk size
  FChunkSize := GetChunkSize;
  WriteSwappedCardinal(Stream, FChunkSize);

  // write chunk name
  Stream.Write(FChunkName, 4);

  // write unknown
  Stream.Write(FUnknown, 4);

  for i := 0 to FChunkList.Count - 1 do
    FChunkList[i].SaveToStream(Stream);

  // insert pad byte if necessary
  if cfPadSize in ChunkFlags then
    Stream.Write(CZeroPad, CalculateZeroPad);
end;


{ TMp4MoovChunk }

constructor TMp4MoovChunk.Create;
begin
  inherited;

  RegisterChunkClasses([TMp4MovieHeaderChunk, TMp4TrackChunk, TMp4UserDataChunk]);
end;

class function TMp4MoovChunk.GetClassChunkName: TChunkName;
begin
  Result := 'moov';
end;


{ TFileMp4 }

constructor TFileMp4.Create;
begin
  inherited;

  FChunkFlags := [cfSizeFirst, cfReversedByteOrder];
  FFileType := TMp4FileTypeChunk.Create;
end;

class function TFileMp4.CanLoad(Stream: TStream): Boolean;
var
  ChunkSize: Cardinal;
  ChunkName: TChunkName;
begin
  Result := Stream.Size >= 8;

  if Result then
  begin
    Stream.Read(ChunkSize, 4);
    Stream.Read(ChunkName, 4);
    Stream.Seek(-8, soFromCurrent);
    Result := ChunkName = GetClassChunkName;
  end;
end;

class function TFileMp4.GetClassChunkName: TChunkName;
begin
  Result := 'ftyp';
end;

procedure TFileMp4.ReadUnknownChunk(Stream: TStream; ChunkName: TChunkName;
  ChunkSize: Cardinal);
var
  ChunkClass: TDefinedChunkClass;
  DefinedChunk: TDefinedChunk;
  UnknownChunk: TMp4UnknownChunk;
begin
  ChunkClass := ChunkClassByChunkName(ChunkName);
  if Assigned(ChunkClass) then
  begin
    DefinedChunk := ChunkClass.Create;
    DefinedChunk.LoadFromStream(Stream);
    FChunkList.Add(DefinedChunk);
  end
  else
  begin
    UnknownChunk := TMp4UnknownChunk.Create;
    UnknownChunk.LoadFromStream(Stream);
    AddChunk(UnknownChunk);
  end;
end;

procedure TFileMp4.LoadFromStream(Stream: TStream);
var
  ChunkSize: Cardinal;
  ChunkName: TChunkName;
begin
  FFileType.LoadFromStream(Stream);
  AddChunk(FFileType);

  while Stream.Position + 8 <= Stream.Size do
  begin
    Stream.Read(ChunkSize, 4);
    Stream.Read(ChunkName, 4);
    Flip32(ChunkSize);
    Stream.Position := Stream.Position - 8;
    ReadUnknownChunk(Stream, ChunkName, ChunkSize);
  end;
end;

procedure TFileMp4.SaveToStream(Stream: TStream);
begin
  inherited;
end;


initialization

  TFileMp4.RegisterChunks([TMp4FileTypeChunk, TMp4FreeChunk, TMp4MovieDataChunk,
    TMp4MoovChunk, TMp4MovieHeaderChunk, TMp4TrackChunk]);

end.
