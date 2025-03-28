import React, { useState, useMemo } from 'react';
import { Delaunay } from 'd3-delaunay';
import * as d3 from 'd3';
import RegionModal from './region/RegionModal'; // Import the RegionModal component

const RegionTooltip = ({ region, latestData, position }) => {
    if (!position) return null;
    
    return (
      <div 
        className="absolute bg-white border border-gray-200 rounded-lg shadow-lg p-4 z-50"
        style={{
          left: position.x + 10,
          top: position.y - 10,
          minWidth: '200px'
        }}
      >
        <h3 className="font-bold mb-2">Region {region.id.slice(0, 4)}</h3>
        <div className="space-y-2 text-sm">
          <p>Population: {region.population}</p>
          <p>Resources: {JSON.stringify(region.storedResources) || 'N/A'}</p>
        </div>
      </div>
    );
  };

const RegionVertex = ({ 
  region,
  onRegionClick,
  viewTransform  // Used for zoom-based styling
}) => {
  const { id, location, population, storedResources } = region;
  const { zoom } = viewTransform;
  const [tooltipPosition, setTooltipPosition] = useState(null);
  
  // Adjust text size and reduce the circle radius (so vertices are less prominent)
  const textSize = Math.max(12 / zoom, 8);
  const circleRadius = Math.max(6 / zoom, 35); // Reduced vertex size

    // Handle mouse events for tooltip
    const handleMouseMove = (e) => {
        console.log("mouseover");
    // Get position relative to the SVG container
    const svgRect = e.currentTarget.ownerSVGElement.getBoundingClientRect();
    setTooltipPosition({
        x: e.clientX - svgRect.left,
        y: e.clientY - svgRect.top
    });
    };

    const handleMouseLeave = () => {
    setTooltipPosition(null);
    };

  return (
    <>
    <g 
      onClick={() => onRegionClick?.(region)}
      onMouseOver={handleMouseMove}
      onMouseLeave={handleMouseLeave}
      className="cursor-pointer"
    >
      <circle
        cx={location.x}
        cy={location.y}
        r={circleRadius}
        fill="white"
        stroke="blue"
        strokeWidth={2 / zoom}
        className="transition-colors hover:fill-blue-100"
      />
      
      <text
        x={location.x}
        y={location.y - 30 / zoom}
        textAnchor="middle"
        style={{ fontSize: textSize }}
        className="font-semibold"
      >
        Region {id.slice(0, 4)}
      </text>
      
      <text
        x={location.x}
        y={location.y - 15 / zoom}
        textAnchor="middle"
        style={{ fontSize: textSize * 0.8 }}
      >
        Pop: {population}
      </text>
      
      {/*<text
        x={location.x}
        y={location.y + 20 / zoom}
        textAnchor="middle"
        style={{ fontSize: textSize * 0.8 }}
      >
        X: {location.x} Y: {location.y}
      </text>*/}
    </g>

  {tooltipPosition && (
    <foreignObject
      x={location.x}
      y={location.y}
      width="100%"
      height="100%"
      style={{ overflow: 'scroll', fontSize: textSize, backgroundColor: "lightgray" }}
    >
      <RegionTooltip
        region={region}
        position={tooltipPosition}
      />
    </foreignObject>
  )}
  </>
  );
};

const RegionsMap = ({ 
  regions, 
  connections = [],
  className = "",
  initialCamera = { x: 0, y: 0, zoom: 0.16 }
}) => {
  // Camera state
  const [camera, setCamera] = useState(initialCamera);
  
  // Modal state
  const [isModalOpen, setIsModalOpen] = useState(false);
  const [selectedRegionId, setSelectedRegionId] = useState(null);
  
  // SVG dimensions for display (viewport)
  const WIDTH = 1200;
  const HEIGHT = 800;

  // Handle region click to open modal
  const handleRegionClick = (region) => {
    setSelectedRegionId(region.id);
    setIsModalOpen(true);
    console.log('Opening modal for region', region.id);
  };

  // Handle modal close
  const handleCloseModal = () => {
    setIsModalOpen(false);
  };

  // Compute the voronoi cells and assign random pastel colors to each cell.
  // The cells are computed from the absolute positions of regions,
  // using a fixed bounding box of [0, 0, 7200, 4200].
  const { voronoi, cellColors } = useMemo(() => {
    if (!regions.length) return { voronoi: null, cellColors: [] };
    
    const points = regions.map(region => [region.location.x, region.location.y]);
    const delaunay = Delaunay.from(points);
    // Calculate voronoi with the full bounds regardless of the current viewport.
    const voronoi = delaunay.voronoi([0, 0, 7200, 4200]);

    // Generate a random pastel color for each point
    const cellColors = points.map(() => {
      const hue = Math.random() * 360;                   // any hue
      const saturation = 30 + Math.random() * 10;          // around 30-40%
      const lightness = 80 + Math.random() * 10;           // around 80-90%
      return d3.hsl(hue, saturation / 100, lightness / 100).toString();
    });
    return { voronoi, cellColors };
  }, [regions]);

  // Build the view transform so that (0,0) in world coordinates is at the top left.
  // The transform maps world coordinates to screen space as: (world - camera) * zoom.
  const viewTransform = {
    transform: `translate(${-camera.x} ${-camera.y}) scale(${camera.zoom})`,
    zoom: camera.zoom
  };

  return (
    <div className={className} style={{ width: WIDTH, height: HEIGHT, position: 'relative' }}>
      <svg 
        className="w-full h-full bg-white"
        width={WIDTH}
        height={HEIGHT}
      >
        <g transform={viewTransform.transform}>
          {/* Draw voronoi cells with fills */}
          {voronoi && regions.map((region, i) => {
            const cell = voronoi.cellPolygon(i);
            if (!cell) return null;
            // Create an SVG path string from the polygon points.
            const d = cell.reduce((acc, point, j) => 
              acc + (j === 0 ? `M${point[0]} ${point[1]}` : `L${point[0]} ${point[1]}`),
              ""
            ) + "Z";
            return (
              <path 
                key={`voronoi-${region.id}`}
                d={d}
                fill={cellColors[i]}
                stroke="red"
                strokeWidth={1 / camera.zoom}
              />
            );
          })}

          {/* Draw connections between regions */}
          {connections.map((connection, index) => {
            const fromRegion = regions.find(r => r.id === connection.from);
            const toRegion = regions.find(r => r.id === connection.to);
            if (!fromRegion || !toRegion) return null;
            return (
              <line
                key={`connection-${index}`}
                x1={fromRegion.location.x}
                y1={fromRegion.location.y}
                x2={toRegion.location.x}
                y2={toRegion.location.y}
                stroke="gray"
                strokeWidth={2 / camera.zoom}
              />
            );
          })}

          {/* Draw region vertices */}
          {regions.map(region => (
            <RegionVertex
              key={region.id}
              region={region}
              viewTransform={viewTransform}
              onRegionClick={handleRegionClick}
            />
          ))}
        </g>

        {/* Debug info (optional) */}
        <text x="10" y="20" className="text-xs">
          Camera: ({Math.round(camera.x)}, {Math.round(camera.y)}) Zoom: {camera.zoom.toFixed(2)}x
        </text>
      </svg>

      {/* Camera controls */}
      <div className="absolute bottom-4 right-4 flex gap-2">
        <button 
          className="px-2 py-1 bg-gray-200 rounded"
          onClick={() => setCamera(prev => ({ ...prev, zoom: prev.zoom * 1.2 }))}
        >
          +
        </button>
        <button 
          className="px-2 py-1 bg-gray-200 rounded"
          onClick={() => setCamera(prev => ({ ...prev, zoom: Math.max(0.16, prev.zoom / 1.2) }))}
        >
          -
        </button>
        <button 
          className="px-2 py-1 bg-gray-200 rounded"
          onClick={() => setCamera({ x: 0, y: 0, zoom: 0.16 })}
        >
          Reset
        </button>
      </div>

      {/* Region Modal */}
      <RegionModal
        isOpen={isModalOpen}
        onClose={handleCloseModal}
        regionId={selectedRegionId}
      />
    </div>
  );
};

export default RegionsMap;