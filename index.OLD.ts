import {
  BigNum,
  DataHash,
  Ed25519KeyHash,
  ScriptHash,
  TransactionHash,
  TransactionInput,
} from "@mlabs-haskell/cardano-serialization-lib-gc";

export const purescript: Promise<
  { [k: string]: any } & Foreign.PureScriptTypes
> = import("./output/Dao.Web.Api");

export namespace Foreign {

  /* CTL and general PS types */

  export declare class WalletSpec {}
  export declare class ConnectToNami extends WalletSpec {}
  
  export interface CurrencySymbol {
    value0: ByteArray;
  }

  export interface TokenName {
    value0: RawBytes;
  }

  export declare class Address {
    addressCredential: Credential;
    addressStakingCredential: Maybe<StakingCredential>;
  }
BigNum
  export declare class StakingCredential {}
  export declare class StakingHash extends StakingCredential {
    value0: Credential;
  }
  export declare class StakingPtr extends StakingCredential {
    slot: Slot;
    txIx: TransactionIndex;
    certIx: CertificateIndex;
  }
  export interface Slot {
    value0: BigNum;
  }
  export interface TransactionIndex {
    value0: BigNum;
  }
  export interface CertificateIndex {
    value0: BigNum;
  }

  export interface ByteArray {
    value0: Uint8Array;
  }

  export interface RawBytes {
    value0: ByteArray;
  }

  export interface TransactionIndex {
    value0: BigNum;
  }

  export declare class Credential {}
  export declare class PubKeyCredential extends Credential {
    value0: PubKeyHash;
  }
  export declare class ScriptCredential extends Credential {
    value0: ValidatorHash;
  }

  export interface PaymentPubKeyHash {
    value0: PubKeyHash;
  }
  export interface PubKeyHash {
    value0: Ed25519KeyHash;
  }

  export interface ValidatorHash {
    value0: ScriptHash;
  }

  export interface POSIXTime {
    value0: BigInteger;
  }

  export declare class Maybe<T> {}
  export declare class Just<T> extends Maybe<T> {
    constructor(value0: T);
    value0: T;
  }
  export declare class Nothing<T> extends Maybe<T> {
    constructor();
  }

  export declare class Either<L, R> {}
  export declare class Left<L, R> extends Either<L, R> {
    constructor(value0: L);
    value0: L;
  }
  export declare class Right<L, R> extends Either<L, R> {
    constructor(value0: R);
    value0: R;
  }

  export declare class Tuple<T1, T2> {
    constructor(value0: T1, value1: T2);
    value0: T1;
    value1: T2;
  }

  export interface AssetClass {
    symbol: Uint8Array;
    name: Uint8Array;
  }

  export type AssocMap<K, V> = Array<Tuple<K, V>>;

  export type PureScriptTypes = {
    WalletSpec: typeof WalletSpec;
    ConnectToNami: typeof ConnectToNami;
    Maybe: typeof Maybe;
    Nothing: typeof Nothing;
    Just: typeof Just;
    mkEnv: (config: ConfigParams) => Promise<ContractEnv>;
    testnetConfig: ConfigParams;
    Tuple: typeof Tuple;
  };

  // This is ContractParams now
  export interface ConfigParams {
    ogmiosConfig: ServerConfig;
    /* put kupo here too? */
    datumCacheConfig: ServerConfig;
    ctlServerConfig: Maybe<ServerConfig>;
    networkId: NetworkId;
    extraConfig: {};
    walletSpec: Maybe<WalletSpec>;
    logLevel: LogLevel;
    suppressLogs: boolean;
  }

  export interface ContractEnv {};

  export interface ServerConfig {
    port: number;
    host: string;
    secure: boolean;
    path: Maybe<string>;
  }

  /* Dao contract params and result types */

  export interface CreateConfigParams
    { configTokenName : string
    , upgradeMajorityPercent : bigint
    , upgradeRelativeMajorityPercent : bigint
    , generalMajorityPercent : bigint
    , generalRelativeMajorityPercent : bigint
    , tripMajorityPercent : bigint
    , tripRelativeMajorityPercent : bigint
    , totalVotes : bigint
    , maxGeneralDisbursement : bigint
    , maxTripDisbursement : bigint
    , agentDisbursementPercent : bigint
    , proposalTallyEndOffset : bigint
    , tallyNft : string // currency symbol
    , voteTokenName : string
    , voteNftSymbol : string
    , voteFungibleCurrencySymbol : string
    , voteFungibleTokenName : string
    , fungibleVotePercent : bigint
    , indexSymbol : string
    , indexTokenName : string
    }

  export interface TreasuryParams {
    configSymbol: CurrencySymbol;
    configTokenName: TokenName;
    tallySymbol: CurrencySymbol;
    treasurySymbol: CurrencySymbol;
  }

  export interface ContractResult {
    txHash: TransactionHash;
    symbol: CurrencySymbol;
    tokenName: TokenName;
  }

  export interface QueryResult {
      proposalTokenName: TokenName;
      tallyDatum: TallyStateDatum;
    }

  export interface VoteOnProposalResult {
    txHash: TransactionHash;
    symbol: CurrencySymbol;
  }

  export interface CreateProposalParams {
    configSymbol: CurrencySymbol;
    indexSymbol: CurrencySymbol;
    configTokenName: TokenName;
    indexTokenName: TokenName;
    tallyStateDatum: TallyStateDatum;
  }

  export interface QueryProposalParams {
    configSymbol: CurrencySymbol;
    indexSymbol: CurrencySymbol;
    configTokenName: TokenName;
    indexTokenName: TokenName;
  }

  export interface CancelVoteParams {
    configSymbol: CurrencySymbol;
    configTokenName: TokenName;
    proposalTokenName: TokenName;
  }

  export interface CountVoteParams {
    configSymbol: CurrencySymbol;
    configTokenName: TokenName;
    tallySymbol: CurrencySymbol;
    proposalTokenName: TokenName;
    voteTokenName: TokenName;
  }

  export interface VoteOnProposalParams {
    configSymbol: CurrencySymbol;
    configTokenName: TokenName;
    tallySymbol: CurrencySymbol;
    voteTokenName: TokenName;
  }

  export interface UpgradeConfigParams {
    newDynamicConfigDatum: DynamicConfigDatum;
    configSymbol: CurrencySymbol;
    configTokenName: TokenName;
    tallySymbol: CurrencySymbol;
  }

  /* App-specific types (Datums etc.) */

  export interface DynamicConfigDatum {
    tallyValidator: ScriptHash
    treasuryValidator: ScriptHash
    configurationValidator: ScriptHash
    voteValidator: ScriptHash
    upgradeMajorityPercent: BigInteger;
    upgradeRelativeMajorityPercent: BigInteger;
    generalMajorityPercent: BigInteger;
    generalRelativeMajorityPercent: BigInteger;
    tripMajorityPercent: BigInteger;
    tripRelativeMajorityPercent: BigInteger;
    totalVotes: BigInteger;
    maxGeneralDisbursement: BigInteger;
    maxTripDisbursement: BigInteger;
    agentDisbursementPercent: BigInteger;
    proposalTallyEndOffset: BigInteger;
    tallyNft: CurrencySymbol;
    voteCurrencySymbol: CurrencySymbol;
    voteTokenName: TokenName;
    voteNft: CurrencySymbol;
    voteFungibleCurrencySymbol: CurrencySymbol;
    voteFungibleTokenName: TokenName;
    fungibleVotePercent: BigInteger;
  }

  export interface TallyStateDatum {
    proposal: ProposalType;
    proposalEndTime: POSIXTime;
    for: BigInteger;
    against: BigInteger;
  }

  export declare class ProposalType {}
  export declare class Upgrade extends ProposalType {
    value0: CurrencySymbol;
  }
  export declare class General extends ProposalType {
    value0: Address;
    value1: BigInteger;
  }
  export declare class Trip extends ProposalType {
    value0: Address;
    value1: Address;
    value2: BigInteger;
  }

  export interface VoteDatum {
    proposalTokenName: TokenName;
    direction: VoteDirection;
    voteOwner: Address;
    returnAda: BigInteger;
  }

  export declare class VoteDirection {};
  export declare class For extends VoteDirection {};
  export declare class Against extends VoteDirection {};
  export interface IndexDatum {
    index: BigInteger;
  }

}

type WalletSpec = "ConnectToNami";

type LogLevel = "Trace";

type NetworkId = "TestnetId";

export const createConfig = async (
  env: Foreign.ContractEnv,
  params: Foreign.CreateConfigParams
): Promise<Foreign.ContractResult> => (await purescript).createConfig(env, params);

export const createIndex = async (
  env: Foreign.ContractEnv,
  params: Foreign.TokenName
): Promise<Foreign.ContractResult> => (await purescript).createIndex(env, params);

export const createProposal = async (
  env: Foreign.ContractEnv,
  params: Foreign.CreateProposalParams
): Promise<Foreign.ContractResult> => (await purescript).createProposal(env, params);

export const createVotePass = async (
  env: Foreign.ContractEnv,
  pkh: Foreign.PaymentPubKeyHash
): Promise<Foreign.ContractResult> => (await purescript).createIndex(env, pkh);

export const voteOnProposal = async (
  env: Foreign.ContractEnv,
  params: Foreign.VoteOnProposalParams
): Promise<Foreign.VoteOnProposalResult> => (await purescript).voteOnProposal(env, params);

export const countVote = async (
  env: Foreign.ContractEnv,
  params: Foreign.CountVoteParams
): Promise<TransactionHash> => (await purescript).countVote(env, params);

export const cancelVote = async (
  env: Foreign.ContractEnv,
  params: Foreign.CancelVoteParams
): Promise<TransactionHash> => (await purescript).cancelVote(env, params);

export const treasuryGeneral = async (
  env: Foreign.ContractEnv,
  params: Foreign.TreasuryParams
): Promise<TransactionHash> => (await purescript).treasuryGeneral(env, params);

export const treasuryTrip = async (
  env: Foreign.ContractEnv,
  params: Foreign.TreasuryParams
): Promise<TransactionHash> => (await purescript).treasuryTrip(env, params);

export const upgradeConfig = async (
  env: Foreign.ContractEnv,
  params: Foreign.UpgradeConfigParams
): Promise<TransactionHash> => (await purescript).upgradeConfig(env, params);

export const getAllProposals = async (
  env: Foreign.ContractEnv,
  params: Foreign.QueryProposalParams
): Promise<Array Foreign.QueryResult> => (await purescript).getAllProposals(env, params);

export const getAllGeneralProposals = async (
  env: Foreign.ContractEnv,
  params: Foreign.QueryProposalParams
): Promise<Array Foreign.QueryResult> => (await purescript).getAllGeneralProposals(env, params);

export const getAllTripProposals = async (
  env: Foreign.ContractEnv,
  params: Foreign.QueryProposalParams
): Promise<Array Foreign.QueryResult> => (await purescript).getAllTripProposals(env, params);

export const getAllUpgradeProposals = async (
  env: Foreign.ContractEnv,
  params: Foreign.QueryProposalParams
): Promise<Array Foreign.QueryResult> => (await purescript).getAllUpgradeProposals(env, params);

export const getAllActiveProposals = async (
  env: Foreign.ContractEnv,
  params: Foreign.QueryProposalParams
): Promise<Array Foreign.QueryResult> => (await purescript).getAllActiveProposals(env, params);

export const getAllExpiredProposals = async (
  env: Foreign.ContractEnv,
  params: Foreign.QueryProposalParams
): Promise<Array Foreign.QueryResult> => (await purescript).getAllExpiredProposals(env, params);

export const getAllSuccessfulProposals = async (
  env: Foreign.ContractEnv,
  params: Foreign.QueryProposalParams
): Promise<Array Foreign.QueryResult> => (await purescript).getAllSuccessfulProposals(env, params);

export const getProposalByTokenName = async (
  env: Foreign.ContractEnv,
  params: Foreign.QueryProposalParams,
  proposalTokenName: Foreign.TokenName
): Promise<Foreign.Maybe Foreign.QueryResult> =>
    (await purescript).getProposalByTokenName(env, params, proposalTokenName);
