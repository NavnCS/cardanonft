package handlers

import (
	"github.com/gofiber/fiber/v2"
)

func GetContractInfo(c *fiber.Ctx) error {
	contract := fiber.Map{
		"policyId":  "abc123xyz456",
		"scriptUrl": "https://ipfs.io/ipfs/Qm...",
	}
	return c.JSON(contract)
}
