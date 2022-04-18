import Document, { Html, Head, Main, NextScript } from "next/document";
import Header from "../components/header";

class MyDocument extends Document {
  render() {
    return (
      <Html>
        <Head>
          <link
            rel="stylesheet"
            href="https://unpkg.com/dracula-prism/dist/css/dracula-prism.css"
          ></link>
          <meta
            name="description"
            content="Haskell GraphQL library, Build GraphQL APIs with your favorite functional language!"
          />
          <meta name="keywords" content="Morpheus GraphQL, GraphQL, Haskell" />
          <link rel="icon" href="/favicon.png" />
        </Head>
        <body>
          <Header />
          <Main />
          <NextScript />
        </body>
      </Html>
    );
  }
}

export default MyDocument;
