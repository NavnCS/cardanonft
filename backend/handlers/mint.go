package handlers

import (
	"log"

	"github.com/gofiber/fiber/v2"
)

type MintRequest struct {
	Name          string `json:"name"`
	Image         string `json:"image"`
	WalletAddress string `json:"walletAddress"`
}

func MintNFT(c *fiber.Ctx) error {
	var req MintRequest
	if err := c.BodyParser(&req); err != nil {
		return c.Status(400).JSON(fiber.Map{"error": "Invalid request"})
	}

	log.Printf("Minting NFT: %+v", req)

	// TODO: Add actual Blockfrost + transaction build/mint logic here
	// For now, simulate success
	return c.JSON(fiber.Map{
		"status":  "success",
		"txHash":  "simulated_tx_hash_abc123",
		"message": "NFT minted (simulated)",
	})
}
