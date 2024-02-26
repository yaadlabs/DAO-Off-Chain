import { BigNum, } from "@mlabs-haskell/cardano-serialization-lib-gc";
export const purescript = import("./output/Dao.Web.Api");
export var Foreign;
(function (Foreign) {
    /* CTL and general PS types */
    BigNum;
    ;
    ;
    ;
    ;
})(Foreign || (Foreign = {}));
export const createConfig = async (env, params) => (await purescript).createConfig(env, params);
export const createIndex = async (env, params) => (await purescript).createIndex(env, params);
export const createProposal = async (env, params) => (await purescript).createProposal(env, params);
export const createVotePass = async (env, pkh) => (await purescript).createIndex(env, pkh);
export const voteOnProposal = async (env, params) => (await purescript).voteOnProposal(env, params);
export const countVote = async (env, params) => (await purescript).countVote(env, params);
export const cancelVote = async (env, params) => (await purescript).cancelVote(env, params);
export const treasuryGeneral = async (env, params) => (await purescript).treasuryGeneral(env, params);
export const treasuryTrip = async (env, params) => (await purescript).treasuryTrip(env, params);
export const upgradeConfig = async (env, params) => (await purescript).upgradeConfig(env, params);
//# sourceMappingURL=index.js.map