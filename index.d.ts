// ====================================
// Contract environment/result
// ====================================

export interface CtlConfig {
  blockfrostApiKey: string;
  network: string;
}

// TODO: complete the ContractEnv interface
export interface ContractEnv {}

export interface ContractResult {
  txHash: string;
  symbol: string;
  tokenName: string;
}

export interface CreateConfigResult extends ContractResult {
  tallySymbol: string;
}

export interface QueryResult {
  proposalTokenName: string;
  tallyDatum: TallyStateDatum;
}

export interface VoteOnProposalResult {
  txHash: string;
  symbol: string;
}

// ====================================
// Contract params
// ====================================

export interface CreateConfigParams {
  configTokenName: string;
  upgradeMajorityPercent: bigint;
  upgradeRelativeMajorityPercent: bigint;
  generalMajorityPercent: bigint;
  generalRelativeMajorityPercent: bigint;
  tripMajorityPercent: bigint;
  tripRelativeMajorityPercent: bigint;
  totalVotes: bigint;
  maxGeneralDisbursement: bigint;
  maxTripDisbursement: bigint;
  agentDisbursementPercent: bigint;
  proposalTallyEndOffset: bigint;
  voteTokenName: string;
  voteNftSymbol: string;
  voteFungibleCurrencySymbol: string;
  voteFungibleTokenName: string;
  fungibleVotePercent: bigint;
  indexSymbol: string;
  indexTokenName: string;
}

export interface CreateProposalParams {
  configSymbol: string;
  indexSymbol: string;
  configTokenName: string;
  indexTokenName: string;
  tallyStateDatum: TallyStateDatum;
}

export interface VoteOnProposalParams {
  configSymbol: string;
  configTokenName: string;
  tallySymbol: string;
  // Vote datum fields
  proposalTokenName: string;
  voteDirection: VoteDirection;
  returnAda: bigint;
}

export declare class VoteDirection {};
export declare class For extends VoteDirection {};
export declare class Against extends VoteDirection {};

export interface QueryProposalParams {
  configSymbol: string;
  indexSymbol: string;
  configTokenName: string;
  indexTokenName: string;
}

// ====================================
// Datums
// ====================================

export interface TallyStateDatum {
  proposal: ProposalType;
  proposalEndTime: bigint;
  for: bigint;
  against: bigint;
}

export declare class ProposalType {}
export declare class Upgrade extends ProposalType {
  constructor(value0: string);
  value0: string; // Currency symbol
}
export declare class General extends ProposalType {
  constructor(value0: string, value1: bigint);
  value0: string; // Address
  value1: bigint;
}
export declare class Trip extends ProposalType {
  constructor(value0: string, value1: string, value2: bigint);
  value0: string; // Address
  value1: string; // Address
  value2: bigint;
}

// ====================================
// Contract environment functions
// ====================================

export declare function initialize(config: CtlConfig): Promise<ContractEnv>;

export declare function finalize(env: ContractEnv): Promise<void>;

// ====================================
// TripHut contract calls
// ====================================

export declare function createConfig(
  env: ContractEnv,
  params: CreateConfigParams
): Promise<CreateConfigResult>;

export declare function createIndex(
  env: ContractEnv,
  tokenName: string
): Promise<ContractResult>;

export declare function createIndexConfig(
  env: ContractEnv
): Promise<CreateConfigResult>;

export declare function createProposal(
  env: ContractEnv,
  params: CreateProposalParams
): Promise<ContractResult>;

export declare function voteOnProposal(
  env: ContractEnv,
  params: VoteOnProposalParams
): Promise<VoteOnProposalResult>;

export declare function getAllProposals(
  env: ContractEnv,
  params: QueryProposalParams
): Promise<QueryResult[]>;

export declare function getAllGeneralProposals(
  env: ContractEnv,
  params: QueryProposalParams
): Promise<QueryResult[]>;

export declare function getAllTripProposals(
  env: ContractEnv,
  params: QueryProposalParams
): Promise<QueryResult[]>;

export declare function getAllUpgradeProposals(
  env: ContractEnv,
  params: QueryProposalParams
): Promise<QueryResult[]>;

export declare function getAllActiveProposals(
  env: ContractEnv,
  params: QueryProposalParams
): Promise<QueryResult[]>;

export declare function getAllExpiredProposals(
  env: ContractEnv,
  params: QueryProposalParams
): Promise<QueryResult[]>;

export declare function getAllSuccessfulProposals(
  env: ContractEnv,
  params: QueryProposalParams
): Promise<QueryResult[]>;

export declare function getProposalByTokenName(
  env: ContractEnv,
  params: QueryProposalParams,
  proposalTokenName: TokenName
): Promise<QueryResult | null>;
