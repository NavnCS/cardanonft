<template>
  <div id="app">
    <h1>Cardano NFT Store (Testnet)</h1>
    <button @click="connectWallet">Connect Nami Wallet</button>

    <div v-if="walletConnected">
      <p><strong>Address:</strong> {{ address }}</p>
      <p><strong>Balance:</strong> {{ balance }} ADA</p>
      <button @click="signDummyTransaction">Sign Dummy Transaction</button>
    </div>
  </div>
</template>

<script setup>
import { ref } from 'vue'

const walletConnected = ref(false)
const address = ref('')
const balance = ref(0)

async function connectWallet() {
  if (!window.cardano || !window.cardano.nami) {
    alert('Nami wallet not found. Please install it.')
    return
  }

  const api = await window.cardano.nami.enable()
  const usedAddresses = await api.getUsedAddresses()
  const hexAddress = usedAddresses[0]
  const addressBytes = await window.cardano.nami.getNetworkId()
  const balanceInLovelace = await api.getBalance()

  const bech32 = await api.getChangeAddress()
  const decoded = await (await import('@emurgo/cardano-serialization-lib-browser')).then(lib => {
    const addr = lib.Address.from_bytes(Buffer.from(bech32, 'hex'))
    return addr.to_bech32()
  })

  address.value = decoded
  balance.value = (parseInt(balanceInLovelace, 16) / 1_000_000).toFixed(2)
  walletConnected.value = true
}

async function signDummyTransaction() {
  alert('Later we will sign a smart contract transaction here.')
}
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
</style>
