import React, { useState, useMemo } from 'react';
import { Delaunay } from 'd3-delaunay';
import * as d3 from 'd3';

const RegionVertex = ({ 
  region,
  onRegionClick,
  viewTransform  // Used for zoom-based styling
}) => {
  const { id, location, population, storedResources } = region;
  const { zoom } = viewTransform;
  
  // Adjust text size and reduce the circle radius (so vertices are less prominent)
  const textSize = Math.max(12 / zoom, 8);
  const circleRadius = Math.max(20 / zoom, 10); // Reduced vertex size

  return (
    <g 
      onClick={() => onRegionClick?.(region)}
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
        y={location.y - 15 / zoom}
        textAnchor="middle"
        style={{ fontSize: textSize }}
        className="font-semibold"
      >
        Region {id.slice(0, 4)}
      </text>
      
      <text
        x={location.x}
        y={location.y + 5 / zoom}
        textAnchor="middle"
        style={{ fontSize: textSize * 0.8 }}
      >
        Pop: {population}
      </text>
      
      <text
        x={location.x}
        y={location.y + 20 / zoom}
        textAnchor="middle"
        style={{ fontSize: textSize * 0.8 }}
      >
        X: {location.x} Y: {location.y}
      </text>
    </g>
  );
};

const RegionsMap = ({ 
  regions, 
  connections = [],
  className = "",
  initialCamera = { x: 0, y: 0, zoom: 1 }
}) => {
  // Camera state
  const [camera, setCamera] = useState(initialCamera);
  
  // SVG dimensions for display (viewport)
  const WIDTH = 1200;
  const HEIGHT = 800;

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
              onRegionClick={() => console.log('Clicked region', region)}
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
          onClick={() => setCamera(prev => ({ ...prev, zoom: prev.zoom / 1.2 }))}
        >
          -
        </button>
        <button 
          className="px-2 py-1 bg-gray-200 rounded"
          onClick={() => setCamera({ x: 0, y: 0, zoom: 1 })}
        >
          Reset
        </button>
      </div>
    </div>
  );
};

export default RegionsMap;
