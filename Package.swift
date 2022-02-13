// swift-tools-version:5.5

import PackageDescription

let package = Package(
        name: "DeliaGame",
        platforms: [.macOS(.v12)],
        products: [
            .executable(name: "scramble", targets: ["scramble"]),
        ],
        dependencies: [
        ],
        targets: [
            .executableTarget(name: "scramble"),
        ]
)
