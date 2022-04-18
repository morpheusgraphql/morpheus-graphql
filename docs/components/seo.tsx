import Head from "next/head";

const SEO: React.FC<{ title: string }> = ({ title }) => (
  <Head>
    <title>{title} | Morpheus GraphQL</title>
  </Head>
);

export default SEO;
