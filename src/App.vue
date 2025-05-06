<template>
  <div id="app">
    <h1>Cardano NFT Store (Testnet)</h1>

    <button @click="connectWallet">
      {{ walletConnected ? 'Wallet Connected ✅' : 'Connect to Lace Wallet' }}
    </button>

    <div v-if="walletConnected">
      <p><strong>Wallet Address:</strong> {{ walletAddress }}</p>
      <p><strong>Balance:</strong> {{ walletBalance }}</p>

      <button @click="signDummyTransaction">Sign Dummy Transaction</button>
      <p v-if="transactionSigned">✅ Transaction signed (simulated)</p>
    </div>

    <div v-if="walletConnected && userNFTs.length">
      <h2>Your NFTs</h2>
      <div v-for="nft in userNFTs" :key="nft.id">
        <p>{{ nft.name }}</p>
        <img :src="nft.image" alt="NFT image" />
      </div>
    </div>

    <div v-if="nftMetadata">
      <h2>{{ nftMetadata.name }}</h2>
      <p>{{ nftMetadata.description }}</p>
      <img :src="nftMetadata.image" alt="NFT image" />
      <p>Price: {{ nftMetadata.priceAda }} ADA</p>
    </div>

    <div v-if="contractInfo">
      <h2>Contract Info</h2>
      <p><strong>Script Address:</strong> {{ contractInfo.scriptAddress }}</p>
    </div>

    <button @click="mintNFT" :disabled="!walletConnected">Mint NFT</button>
    <button @click="purchaseNFT" :disabled="!walletConnected">Purchase NFT</button>
  </div>
</template>

<script setup>
import { ref, onMounted } from 'vue'
import axios from 'axios'

// Refs
const walletConnected = ref(false)
const walletAddress = ref('')
const walletBalance = ref('Loading...')
const transactionSigned = ref(false)
const nftMetadata = ref(null)
const contractInfo = ref(null)
const userNFTs = ref([])

// Connect to Lace wallet
async function connectWallet() {
  if (window.cardano && window.cardano.lace) {
    try {
      const laceApi = await window.cardano.lace.enable()
      walletConnected.value = true

      const usedAddresses = await laceApi.getUsedAddresses()
      walletAddress.value = usedAddresses[0] || 'Not Found'

      await fetchBalance(laceApi)
      await fetchUserNFTs()
    } catch (error) {
      console.error('Wallet connection failed:', error)
      alert('Failed to connect wallet.')
    }
  } else {
    alert('Lace wallet not found.')
  }
}

// Fetch wallet balance
async function fetchBalance(api) {
  try {
    const balance = await api.getBalance()
    const ada = BigInt(balance).toString()
    walletBalance.value = `${Number(ada) / 1_000_000} ADA`
  } catch (error) {
    console.error('Error fetching balance:', error)
    walletBalance.value = 'Error'
  }
}

// Dummy sign
async function signDummyTransaction() {
  try {
    console.log('Simulating transaction signing...')
    transactionSigned.value = true
    alert('✅ Transaction signing simulated.')
  } catch (error) {
    console.error('Transaction signing failed:', error)
    alert('❌ Transaction signing failed.')
  }
}

// Mint NFT
async function mintNFT() {
  try {
    const response = await axios.post('http://localhost:8080/api/mint', {
      walletAddress: walletAddress.value,
      name: "WiraNFT #3",
      image: "https://example.com/nft3.png"
    })
    console.log("Mint result:", response.data)
    await fetchUserNFTs()
  } catch (error) {
    console.error("Minting failed", error)
    alert('❌ Minting failed.')
  }
}

// Purchase NFT
async function purchaseNFT() {
  try {
    const response = await axios.post("http://localhost:8080/api/purchase", {
      walletAddress: walletAddress.value,
      nftId: "1"
    })
    alert(`✅ Purchase successful! TX Hash: ${response.data.txHash}`)
    await fetchUserNFTs()
  } catch (error) {
    console.error("Purchase failed", error)
    alert("❌ Purchase failed.")
  }
}

// Fetch NFT metadata
async function fetchNFTMetadata() {
  try {
    const response = await axios.get('http://localhost:8080/api/nft')
    nftMetadata.value = response.data
  } catch (error) {
    console.error('Error fetching NFT metadata:', error)
  }
}

// Fetch contract info
async function fetchContractInfo() {
  try {
    const response = await axios.get('http://localhost:8080/api/contract')
    contractInfo.value = response.data
  } catch (error) {
    console.error('Error fetching contract info:', error)
  }
}

// Fetch user NFTs
async function fetchUserNFTs() {
  if (!walletAddress.value) return
  try {
    const response = await axios.get(`http://localhost:8080/api/user-nfts?wallet=${walletAddress.value}`)
    userNFTs.value = response.data
  } catch (error) {
    console.error('Failed to fetch user NFTs', error)
  }
}

// Load data on mount
onMounted(async () => {
  await fetchNFTMetadata()
  await fetchContractInfo()
})
</script>

<style scoped>
body {
  font-family: Arial, sans-serif;
  background: #f5f5f5;
  padding: 2em;
}
button {
  padding: 0.5em 1em;
  margin: 1em 0;
}
img {
  max-width: 200px;
  border-radius: 10px;
  margin: 1em 0;
}
</style>
