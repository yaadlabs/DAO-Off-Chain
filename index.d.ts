import { BigNum, Ed25519KeyHash, ScriptHash, TransactionHash } from "@mlabs-haskell/cardano-serialization-lib-gc";
export declare const purescript: Promise<{
    [k: string]: any;
} & Foreign.PureScriptTypes>;
export declare namespace Foreign {
    class WalletSpec {
    }
    class ConnectToNami extends WalletSpec {
    }
    interface CurrencySymbol {
        value0: ByteArray;
    }
    interface TokenName {
        value0: RawBytes;
    }
    class Address {
        addressCredential: Credential;
        addressStakingCredential: Maybe<StakingCredential>;
    }
    class StakingCredential {
    }
    class StakingHash extends StakingCredential {
        value0: Credential;
    }
    class StakingPtr extends StakingCredential {
        slot: Slot;
        txIx: TransactionIndex;
        certIx: CertificateIndex;
    }
    interface Slot {
        value0: BigNum;
    }
    interface TransactionIndex {
        value0: BigNum;
    }
    interface CertificateIndex {
        value0: BigNum;
    }
    interface ByteArray {
        value0: Uint8Array;
    }
    interface RawBytes {
        value0: ByteArray;
    }
    interface TransactionIndex {
        value0: BigNum;
    }
    class Credential {
    }
    class PubKeyCredential extends Credential {
        value0: PubKeyHash;
    }
    class ScriptCredential extends Credential {
        value0: ValidatorHash;
    }
    interface PaymentPubKeyHash {
        value0: PubKeyHash;
    }
    interface PubKeyHash {
        value0: Ed25519KeyHash;
    }
    interface ValidatorHash {
        value0: ScriptHash;
    }
    interface POSIXTime {
        value0: BigInteger;
    }
    class Maybe<T> {
    }
    class Just<T> extends Maybe<T> {
        constructor(value0: T);
        value0: T;
    }
    class Nothing<T> extends Maybe<T> {
        constructor();
    }
    class Either<L, R> {
    }
    class Left<L, R> extends Either<L, R> {
        constructor(value0: L);
        value0: L;
    }
    class Right<L, R> extends Either<L, R> {
        constructor(value0: R);
        value0: R;
    }
    class Tuple<T1, T2> {
        constructor(value0: T1, value1: T2);
        value0: T1;
        value1: T2;
    }
    interface AssetClass {
        symbol: Uint8Array;
        name: Uint8Array;
    }
    type AssocMap<K, V> = Array<Tuple<K, V>>;
    type PureScriptTypes = {
        WalletSpec: typeof WalletSpec;
        ConnectToNami: typeof ConnectToNami;
        Maybe: typeof Maybe;
        Nothing: typeof Nothing;
        Just: typeof Just;
        mkEnv: (config: ConfigParams) => Promise<ContractEnv>;
        testnetConfig: ConfigParams;
        Tuple: typeof Tuple;
    };
    interface ConfigParams {
        ogmiosConfig: ServerConfig;
        datumCacheConfig: ServerConfig;
        ctlServerConfig: Maybe<ServerConfig>;
        networkId: NetworkId;
        extraConfig: {};
        walletSpec: Maybe<WalletSpec>;
        logLevel: LogLevel;
        suppressLogs: boolean;
    }
    interface ContractEnv {
    }
    interface ServerConfig {
        port: number;
        host: string;
        secure: boolean;
        path: Maybe<string>;
    }
    interface CreateConfigParams {
        configTokenName: TokenName;
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
        voteTokenName: TokenName;
        voteNftSymbol: CurrencySymbol;
        voteFungibleCurrencySymbol: CurrencySymbol;
        voteFungibleTokenName: TokenName;
        fungibleVotePercent: BigInteger;
        indexSymbol: CurrencySymbol;
        indexTokenName: TokenName;
    }
    interface TreasuryParams {
        configSymbol: CurrencySymbol;
        configTokenName: TokenName;
        tallySymbol: CurrencySymbol;
        treasurySymbol: CurrencySymbol;
    }
    interface ContractResult {
        txHash: TransactionHash;
        symbol: CurrencySymbol;
        tokenName: TokenName;
    }
    interface VoteOnProposalResult {
        txHash: TransactionHash;
        symbol: CurrencySymbol;
    }
    interface CreateProposalParams {
        configSymbol: CurrencySymbol;
        indexSymbol: CurrencySymbol;
        configTokenName: TokenName;
        indexTokenName: TokenName;
        tallyStateDatum: TallyStateDatum;
    }
    interface CancelVoteParams {
        configSymbol: CurrencySymbol;
        configTokenName: TokenName;
        proposalTokenName: TokenName;
    }
    interface CountVoteParams {
        configSymbol: CurrencySymbol;
        configTokenName: TokenName;
        tallySymbol: CurrencySymbol;
        proposalTokenName: TokenName;
        voteTokenName: TokenName;
    }
    interface VoteOnProposalParams {
        configSymbol: CurrencySymbol;
        configTokenName: TokenName;
        tallySymbol: CurrencySymbol;
        voteTokenName: TokenName;
    }
    interface UpgradeConfigParams {
        newDynamicConfigDatum: DynamicConfigDatum;
        configSymbol: CurrencySymbol;
        configTokenName: TokenName;
        tallySymbol: CurrencySymbol;
    }
    interface DynamicConfigDatum {
        tallyValidator: ScriptHash;
        treasuryValidator: ScriptHash;
        configurationValidator: ScriptHash;
        voteValidator: ScriptHash;
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
    interface TallyStateDatum {
        proposal: ProposalType;
        proposalEndTime: POSIXTime;
        for: BigInteger;
        against: BigInteger;
    }
    class ProposalType {
    }
    class Upgrade extends ProposalType {
        value0: CurrencySymbol;
    }
    class General extends ProposalType {
        value0: Address;
        value1: BigInteger;
    }
    class Trip extends ProposalType {
        value0: Address;
        value1: Address;
        value2: BigInteger;
    }
    interface VoteDatum {
        proposalTokenName: TokenName;
        direction: VoteDirection;
        voteOwner: Address;
        returnAda: BigInteger;
    }
    class VoteDirection {
    }
    class For extends VoteDirection {
    }
    class Against extends VoteDirection {
    }
    interface IndexDatum {
        index: BigInteger;
    }
}
declare type LogLevel = "Trace";
declare type NetworkId = "TestnetId";
export declare const createConfig: (env: Foreign.ContractEnv, params: Foreign.CreateConfigParams) => Promise<Foreign.ContractResult>;
export declare const createIndex: (env: Foreign.ContractEnv, params: Foreign.TokenName) => Promise<Foreign.ContractResult>;
export declare const createProposal: (env: Foreign.ContractEnv, params: Foreign.CreateProposalParams) => Promise<Foreign.ContractResult>;
export declare const createVotePass: (env: Foreign.ContractEnv, pkh: Foreign.PaymentPubKeyHash) => Promise<Foreign.ContractResult>;
export declare const voteOnProposal: (env: Foreign.ContractEnv, params: Foreign.VoteOnProposalParams) => Promise<Foreign.VoteOnProposalResult>;
export declare const countVote: (env: Foreign.ContractEnv, params: Foreign.CountVoteParams) => Promise<any>;
export declare const cancelVote: (env: Foreign.ContractEnv, params: Foreign.CancelVoteParams) => Promise<any>;
export declare const treasuryGeneral: (env: Foreign.ContractEnv, params: Foreign.TreasuryParams) => Promise<any>;
export declare const treasuryTrip: (env: Foreign.ContractEnv, params: Foreign.TreasuryParams) => Promise<any>;
export declare const upgradeConfig: (env: Foreign.ContractEnv, params: Foreign.UpgradeConfigParams) => Promise<any>;
export {};
