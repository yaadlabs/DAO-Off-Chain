// Used for frontend library
"use strict";

const path = require("path");
const webpack = require("webpack");
const NodePolyfillPlugin = require("node-polyfill-webpack-plugin");

module.exports = {
  mode: "development",

  experiments: {
    asyncWebAssembly: false,
    layers: false,
    lazyCompilation: false,
    outputModule: true,
    syncWebAssembly: true,
    topLevelAwait: true,
  },

  devtool: "eval-source-map",

  stats: { errorDetails: true },

  devServer: {
    port: 4008,
    headers: {
      "Access-Control-Allow-Origin": "*",
      "Access-Control-Allow-Methods": "GET, POST, PUT, DELETE, PATCH, OPTIONS",
      "Access-Control-Allow-Headers": "X-Requested-With, Authorization",
    },
  },

  // The TS entry point
  entry: "./index.ts",

  output: {
    path: path.resolve(__dirname, "dist"),
    filename: "index.js",
    library: {
      type: "module",
    },
  },

  module: {
    rules: [
      {
        test: /\.(png|jpg|gif)$/i,
        use: [
          {
            loader: "url-loader",
            options: {
              limit: 8192,
            },
          },
        ],
      },
      {
        test: /\.wasm$/,
        use: [
          { loader: "wasm-loader",
          },
        ],
      },
      {
        test: /\.tsx?$/,
        use: [
          {
            loader: "ts-loader",
            options: {
              compilerOptions: {
                paths: {
                  "*": [process.env.NODE_PATH + "/*"],
                },
              },
            },
          },
        ],
        exclude: /node_modules/,
      },
    ],
  },

  resolveLoader: {
    modules: [process.env.NODE_PATH],
  },

  resolve: {
    modules: [process.env.NODE_PATH],
    extensions: [".tsx", ".ts", ".js"],
    fallback: {
      buffer: require.resolve("buffer/"),
      http: false,
      url: false,
      stream: false,
      crypto: false,
      https: false,
      net: false,
      tls: false,
      zlib: false,
      os: false,
      path: false,
      fs: false,
    },
  },

  plugins: [
    new webpack.DefinePlugin({
      BROWSER_RUNTIME: !!process.env.BROWSER_RUNTIME,
      WEBPACK: true,      
    }),
    new NodePolyfillPlugin(),
    new webpack.LoaderOptionsPlugin({
      debug: true,
    }),
    new webpack.ProvidePlugin({
      Buffer: ["buffer", "Buffer"],
    }),
    new webpack.ContextReplacementPlugin(/cardano-serialization-lib-browser/),
    new webpack.ContextReplacementPlugin(/cardano-serialization-lib-nodejs/),
  ],
};
