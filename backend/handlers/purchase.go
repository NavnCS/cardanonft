package handlers

import (
	"github.com/gofiber/fiber/v2"
)

func PurchaseNFT(c *fiber.Ctx) error {
	type PurchaseRequest struct {
		WalletAddress string `json:"walletAddress"`
		NFTID         string `json:"nftId"`
	}

	var req PurchaseRequest
	if err := c.BodyParser(&req); err != nil {
		return c.Status(400).JSON(fiber.Map{"error": "Invalid request"})
	}

	return c.JSON(fiber.Map{
		"message": "NFT purchased successfully!",
		"txHash":  "mocked_tx_hash_0x123456",
		"nftId":   req.NFTID,
		"wallet":  req.WalletAddress,
	})
}
