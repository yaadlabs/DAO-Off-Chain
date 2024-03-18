// ====================================
// Contract environment/result
// ====================================

export interface CtlConfig {
  blockfrostApiKey: string;
  network: string;
}

// TODO: complete the ContractEnv interface
export interface ContractEnv {};

export interface ContractResult {
  txHash: string;
  symbol: string;
  tokenName: string;
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

// ====================================
// Contract environment functions
// ====================================

export declare const initialize = (config: CtlConfig) => Promise<ContractEnv>

export declare const finalize = (env: ContractEnv) => Promise<void>

// ====================================
// TripHut contract calls
// ====================================

export declare const createConfig = (env: ContractEnv, params: CreateConfigParams) => Promise<ContractResult>;

export declare const createIndex = (env: ContractEnv, tokenName: string) => Promise<ContractResult>;

export declare const createIndexConfig = (env: ContractEnv) => Promise<ContractResult>;
