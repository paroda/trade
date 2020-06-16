# My personal trade bot in Clojure to do live trading with my custom strategy

A tool to help make profit in forex trading.

Integrated to Forex Connect API from FXCM.

Already defined some basic technical indicators like RSI, ADX, MACD, CCI, ATR. Pretty simple to add even more.

Visualization is available as a simple web server serving pages with SVG graphics. Extending to more SVG pages is quite simple.

A basic backtesting with visualization is available.

The idea is define indicators as transducers. Then candles are to fetched from broker API and put to a channel created with these transducers.
Then define your strategy as a simple function based on those indicators to create buy or sell signals. And using the broker API place the order.
That's it. Now you got your agent running live unattended making the orders based on your logic. You can anytime check the status in a browser.

With the backtesting facility, test your strategy with historical prices obtained from the broker API and when satisfied, use that strategy for live trading.

## Backtest with price history

![backtest screenshot](./docs/images/backtest.png "")
