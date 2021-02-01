unit CfeChunkMp4;

interface

uses
  Classes, SysUtils, CfeChunkCommon;

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

  TMp4TrackHeader = record
    Version: Byte;
    Flags: array [0..2] of Byte;
    CreationTime: Cardinal;
    ModificationTime: Cardinal;
    TrackID: Cardinal;
    ReservedA: Cardinal;
    Duration: Cardinal;
    ReservedB: array [0..7] of Byte;
    Layer: Word;
    AlternateGroup: Word;
    Volume: Word;
    ReservedC: Word;
    MatrixStructure: array [0..8] of Integer;
    TrackWidth: Cardinal;
    TrackHeight: Cardinal;
  end;

  TMp4TrackHeaderChunk = class(TMp4FixedDefinedChunk)
  private
    FTrackHeader: TMp4TrackHeader;
    function GetCreationTime: TDateTime;
    function GetModificationTime: TDateTime;
  public
    constructor Create; override;

    class function GetClassChunkName: TChunkName; override;
    class function GetClassChunkSize: Cardinal; override;

    property CreationTime: TDateTime read GetCreationTime;
    property ModificationTime: TDateTime read GetModificationTime;
  end;

  TMp4EditList = record
    Version: Byte;
    Flags: array [0..2] of Byte;
    CreationTime: Cardinal;
    ModificationTime: Cardinal;
    TrackID: Cardinal;
    ReservedA: Cardinal;
    Duration: Cardinal;
    ReservedB: array [0..7] of Byte;
    Layer: Word;
    AlternateGroup: Word;
    Volume: Word;
    ReservedC: Word;
    MatrixStructure: array [0..8] of Integer;
    TrackWidth: Cardinal;
    TrackHeight: Cardinal;
  end;

  TMp4EditListChunk = class(TMp4FixedDefinedChunk)
  private
    FEditList: TMp4EditList;
  public
    constructor Create; override;

    class function GetClassChunkName: TChunkName; override;
    class function GetClassChunkSize: Cardinal; override;
  end;

  TMp4EditChunk = class(TMp4ContainerChunk)
  public
    constructor Create; override;

    class function GetClassChunkName: TChunkName; override;
  end;

  TMp4MediaHeader = record
    Version: Byte;
    Flags: array [0..2] of Byte;
    CreationTime: Cardinal;
    ModificationTime: Cardinal;
    TimeScale: Cardinal;
    Duration: Cardinal;
    Language: Word;
    Quality: Word;
  end;

  TMp4MediaHeaderChunk = class(TMp4FixedDefinedChunk)
  private
    FMediaHeader: TMp4MediaHeader;
    function GetCreationTime: TDateTime;
    function GetDuration: Single;
    function GetModificationTime: TDateTime;
    function GetTimeScale: Single;
  public
    constructor Create; override;

    class function GetClassChunkName: TChunkName; override;
    class function GetClassChunkSize: Cardinal; override;

    property CreationTime: TDateTime read GetCreationTime;
    property ModificationTime: TDateTime read GetModificationTime;
    property TimeScale: Single read GetTimeScale;
    property Duration: Single read GetDuration;
  end;

  TMp4HandlerReference = record
    Version: Byte;
    Flags: array [0..2] of Byte;
    ComponentType: TChunkName;
    ComponentSubType: TChunkName;
    ComponentManufacturer: TChunkName;
    ComponentFlags: array [0..3] of Byte;
    ComponentFlagMask: array [0..3] of Byte;
  end;

  TMp4HandlerReferenceChunk = class(TMp4FixedDefinedChunk)
  private
    FHandlerReference: TMp4HandlerReference;
    function GetComponentManufacturer: TChunkName;
    function GetComponentSubType: TChunkName;
    function GetComponentType: TChunkName;
  public
    constructor Create; override;

    class function GetClassChunkName: TChunkName; override;
    class function GetClassChunkSize: Cardinal; override;

    property ComponentType: TChunkName read GetComponentType;
    property ComponentSubType: TChunkName read GetComponentSubType;
    property ComponentManufacturer: TChunkName read GetComponentManufacturer;
  end;

  TMp4VideoMediaInformation = record
    Version: Byte;
    Flags: array [0..2] of Byte;
    GraphicsMode: Word;
    OpColor: array [0..2] of Word;
  end;

  TMp4VideoMediaInformationChunk = class(TMp4FixedDefinedChunk)
  private
    FVideoMediaInformation: TMp4VideoMediaInformation;
  public
    constructor Create; override;

    class function GetClassChunkName: TChunkName; override;
    class function GetClassChunkSize: Cardinal; override;
  end;

  TMp4SoundMediaInformation = record
    Version: Byte;
    Flags: array [0..2] of Byte;
    Balance: Word;
    Reserved: Word;
  end;

  TMp4SoundMediaInformationChunk = class(TMp4FixedDefinedChunk)
  private
    FSoundMediaInformation: TMp4SoundMediaInformation;
  public
    constructor Create; override;

    class function GetClassChunkName: TChunkName; override;
    class function GetClassChunkSize: Cardinal; override;
  end;

  TMp4BaseMediaInformation = record
    Version: Byte;
    Flags: array [0..2] of Byte;
    GraphicsMode: Word;
    OpColor: array [0..2] of Word;
    Balance: Word;
    Reserved: Word;
  end;

  TMp4BaseMediaInformationChunk = class(TMp4FixedDefinedChunk)
  private
    FBaseMediaInformation: TMp4BaseMediaInformation;
  public
    constructor Create; override;

    class function GetClassChunkName: TChunkName; override;
    class function GetClassChunkSize: Cardinal; override;
  end;

  TMp4DataReference = record
    Version: Byte;
    Flags: array [0..2] of Byte;
    NumberOfEntries: Cardinal;
  end;

  TMp4DataReferenceChunk = class(TMp4ContainerChunk)
  private
    FDataReference: TMp4DataReference;
  public
    constructor Create; override;

    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  end;

  TMp4DataInformationChunk = class(TMp4ContainerChunk)
  public
    constructor Create; override;

    class function GetClassChunkName: TChunkName; override;
  end;

  TMp4SampleDescription = record
    Version: Byte;
    Flags: array [0..2] of Byte;
    NumberOfEntries: Cardinal;
  end;

  TMp4SampleDescriptionChunk = class(TMp4FixedDefinedChunk)
  private
    FSampleDescription: TMp4SampleDescription;
  public
    constructor Create; override;

    class function GetClassChunkName: TChunkName; override;
    class function GetClassChunkSize: Cardinal; override;
  end;

  TMp4TimeToSample = record
    Version: Byte;
    Flags: array [0..2] of Byte;
    NumberOfEntries: Cardinal;
  end;

  TMp4TimeToSampleChunk = class(TMp4FixedDefinedChunk)
  private
    FTimeToSample: TMp4TimeToSample;
  public
    constructor Create; override;

    class function GetClassChunkName: TChunkName; override;
    class function GetClassChunkSize: Cardinal; override;
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
    destructor Destroy; override;

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
begin
  inherited;

end;


{ TMp4TrackHeaderChunk }

constructor TMp4TrackHeaderChunk.Create;
begin
  inherited;

  StartAddress := @FTrackHeader;
end;

class function TMp4TrackHeaderChunk.GetClassChunkName: TChunkName;
begin
  Result := 'tkhd';
end;

class function TMp4TrackHeaderChunk.GetClassChunkSize: Cardinal;
begin
  Result := SizeOf(TMp4TrackHeaderChunk);
end;

function TMp4TrackHeaderChunk.GetCreationTime: TDateTime;
begin
  Result := IncSecond(EncodeDateTime(1904, 1, 1, 0, 0, 0, 0), FTrackHeader.CreationTime);
end;

function TMp4TrackHeaderChunk.GetModificationTime: TDateTime;
begin
  Result := IncSecond(EncodeDateTime(1904, 1, 1, 0, 0, 0, 0), FTrackHeader.ModificationTime);
end;


{ TMp4EditListChunk }

constructor TMp4EditListChunk.Create;
begin
  inherited;

  StartAddress := @FEditList;
end;

class function TMp4EditListChunk.GetClassChunkName: TChunkName;
begin
  Result := 'elst';
end;

class function TMp4EditListChunk.GetClassChunkSize: Cardinal;
begin
  Result := SizeOf(TMp4EditList);
end;


{ TMp4EditChunk }

constructor TMp4EditChunk.Create;
begin
  inherited;

  RegisterChunkClasses([TMp4EditListChunk]);
end;

class function TMp4EditChunk.GetClassChunkName: TChunkName;
begin
  Result := 'edts';
end;


{ TMp4MediaHeaderChunk }

constructor TMp4MediaHeaderChunk.Create;
begin
  inherited;

  StartAddress := @FMediaHeader;
end;

class function TMp4MediaHeaderChunk.GetClassChunkName: TChunkName;
begin
  Result := 'mdhd';
end;

class function TMp4MediaHeaderChunk.GetClassChunkSize: Cardinal;
begin
  Result := SizeOf(TMp4MediaHeader);
end;

function TMp4MediaHeaderChunk.GetCreationTime: TDateTime;
begin
  Result := IncSecond(EncodeDateTime(1904, 1, 1, 0, 0, 0, 0), FMediaHeader.CreationTime);
end;

function TMp4MediaHeaderChunk.GetModificationTime: TDateTime;
begin
  Result := IncSecond(EncodeDateTime(1904, 1, 1, 0, 0, 0, 0), FMediaHeader.ModificationTime);
end;

function TMp4MediaHeaderChunk.GetDuration: Single;
begin
  Result :=  FMediaHeader.Duration / FMediaHeader.TimeScale;
end;

function TMp4MediaHeaderChunk.GetTimeScale: Single;
begin
  Result :=  1 / FMediaHeader.TimeScale;
end;


{ TMp4HandlerReferenceChunk }

constructor TMp4HandlerReferenceChunk.Create;
begin
  inherited;

  StartAddress := @FHandlerReference;
end;

class function TMp4HandlerReferenceChunk.GetClassChunkName: TChunkName;
begin
  Result := 'hdlr';
end;

class function TMp4HandlerReferenceChunk.GetClassChunkSize: Cardinal;
begin
  Result := SizeOf(TMp4HandlerReference);
end;

function TMp4HandlerReferenceChunk.GetComponentManufacturer: TChunkName;
begin
  Result := FHandlerReference.ComponentManufacturer;
end;

function TMp4HandlerReferenceChunk.GetComponentSubType: TChunkName;
begin
  Result := FHandlerReference.ComponentSubType;
end;

function TMp4HandlerReferenceChunk.GetComponentType: TChunkName;
begin
  Result := FHandlerReference.ComponentType;
end;


{ TMp4VideoMediaInformationChunk }

constructor TMp4VideoMediaInformationChunk.Create;
begin
  inherited;

  StartAddress := @FVideoMediaInformation;
end;

class function TMp4VideoMediaInformationChunk.GetClassChunkName: TChunkName;
begin
  Result := 'vmhd'
end;

class function TMp4VideoMediaInformationChunk.GetClassChunkSize: Cardinal;
begin
  Result := SizeOf(TMp4VideoMediaInformation);
end;


{ TMp4SoundMediaInformationChunk }

constructor TMp4SoundMediaInformationChunk.Create;
begin
  inherited;

  StartAddress := @FSoundMediaInformation;
end;

class function TMp4SoundMediaInformationChunk.GetClassChunkName: TChunkName;
begin
  Result := 'smhd'
end;

class function TMp4SoundMediaInformationChunk.GetClassChunkSize: Cardinal;
begin
  Result := SizeOf(TMp4SoundMediaInformation);
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

constructor TMp4BaseMediaInformationChunk.Create;
begin
  inherited;

  StartAddress := @FBaseMediaInformation;
end;

class function TMp4BaseMediaInformationChunk.GetClassChunkName: TChunkName;
begin
  Result := 'gmin';
end;

class function TMp4BaseMediaInformationChunk.GetClassChunkSize: Cardinal;
begin
  Result := SizeOf(TMp4BaseMediaInformation);
end;


{ TMp4DataReferenceChunk }

constructor TMp4DataReferenceChunk.Create;
begin
  inherited;

end;

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
  Stream.Read(FDataReference.Version, 1);

  // read flags
  Stream.Read(FDataReference.Flags, 3);

  // read number of entries
  Stream.Read(FDataReference.NumberOfEntries, 4);
  Flip32(FDataReference.NumberOfEntries);

  for Index := 0 to FDataReference.NumberOfEntries - 1 do
  begin
    // read chunk size
    Stream.Read(SubChunkSize, 4);
    Flip32(SubChunkSize);

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
  Stream.Write(FDataReference.Version, 1);

  // write flags
  Stream.Write(FDataReference.Flags, 3);

  // write number of entries
  Value := FDataReference.NumberOfEntries;
  Flip32(Value);
  Stream.Write(Value, 4);

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


{ TMp4SampleDescriptionChunk }

constructor TMp4SampleDescriptionChunk.Create;
begin
  inherited;

  StartAddress := @FSampleDescription;
end;

class function TMp4SampleDescriptionChunk.GetClassChunkName: TChunkName;
begin
  Result := 'stsd';
end;

class function TMp4SampleDescriptionChunk.GetClassChunkSize: Cardinal;
begin
  Result := SizeOf(TMp4SampleDescription);
end;


{ TMp4TimeToSampleChunk }

constructor TMp4TimeToSampleChunk.Create;
begin
  inherited;

  StartAddress := @FTimeToSample;
end;

class function TMp4TimeToSampleChunk.GetClassChunkName: TChunkName;
begin
  Result := 'stts';
end;

class function TMp4TimeToSampleChunk.GetClassChunkSize: Cardinal;
begin
  Result := SizeOf(TMp4TimeToSample);
end;


{ TMp4SampleTableChunk }

constructor TMp4SampleTableChunk.Create;
begin
  inherited;

  RegisterChunkClasses([TMp4SampleDescriptionChunk]);
end;

class function TMp4SampleTableChunk.GetClassChunkName: TChunkName;
begin
  Result := 'stbl';
end;


{ TMp4MediaChunk }

constructor TMp4MediaChunk.Create;
begin
  inherited;

  RegisterChunkClasses([TMp4MediaHeaderChunk, TMp4HandlerReferenceChunk, TMp4MediaInformationChunk]);
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
var
  BytesReaded: Cardinal;
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
var
  BytesWritten: Cardinal;
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
  Index: Integer;
  ChunkEnd: Cardinal;
  SubChunkName: TChunkName;
  SubChunkSize: Cardinal;
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
  Value := FChunkSize;
  Flip32(Value);
  Stream.Write(Value, 4);

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

destructor TFileMp4.Destroy;
begin

  inherited;
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
  UnknownChunk: TMp4UnknownChunk;
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

  TFileMp4.RegisterChunks([TMp4FileTypeChunk, TMp4FreeChunk, TMp4MoovChunk,
    TMp4MovieHeaderChunk, TMp4TrackChunk]);

end.
