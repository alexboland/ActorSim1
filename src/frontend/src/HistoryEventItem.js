import React from 'react';

const HistoryEventItem = ({ event }) => {
  // Get color based on event type
  const getEventColor = (eventType) => {
    switch (eventType.name) {
      case 'ResourceProduced':
        return '#C8E6C9'; // Light green
      case 'ResourceConsumed':
        return '#FFCCBC'; // Light red
      case 'PopulationChanged':
        return '#BBDEFB'; // Light blue
      case 'MarketTransaction':
        return '#E1BEE7'; // Light purple
      case 'BondIssued':
        return '#FFE0B2'; // Light orange
      case 'BondRepaid':
        return '#FFF9C4'; // Light yellow
      case 'WorkerHired':
        return '#D1C4E9'; // Light lavender
      case 'FounderCreated':
        return '#F8BBD0'; // Light pink
      case 'ConstructionStarted':
        return '#B2DFDB'; // Light teal
      case 'ConstructionCompleted':
        return '#DCEDC8'; // Light lime
      case 'SeasonChanged':
        return '#CFD8DC'; // Light blue grey
      default:
        return '#F5F5F5'; // Light grey for custom events
    }
  };

  // Format date from timestamp
  const formatDate = (timestamp) => {
    const date = new Date(timestamp);
    return date.toLocaleString();
  };

  const backgroundColor = getEventColor(event.eventType);

  return (
    <div
      className="history-event-item"
      style={{
        backgroundColor,
        padding: '15px',
        marginBottom: '10px',
        borderRadius: '5px',
        boxShadow: '0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24)',
        transition: 'all 0.3s cubic-bezier(.25,.8,.25,1)',
        position: 'relative'
      }}
    >
      <div className="event-header" style={{
        display: 'flex',
        justifyContent: 'space-between',
        marginBottom: '8px',
        fontWeight: 'bold'
      }}>
        <span className="event-type">
          {event.eventType.name}
        </span>
        <span className="event-time">
          {formatDate(event.timestamp)}
        </span>
      </div>

      <div className="event-content" style={{ marginBottom: '10px' }}>
        <p style={{ margin: '0 0 5px 0' }}>{event.eventText}</p>
      </div>

      <div className="event-footer" style={{
        display: 'flex',
        fontSize: '0.85rem',
        color: '#666',
        gap: '15px'
      }}>
        <div>
          <strong>Agent:</strong> {event.agentId}
        </div>
        <div>
          <strong>Region:</strong> {event.regionId}
        </div>
        {event.formattedTime && (
          <div>
            <strong>Game Time:</strong> {event.formattedTime}
          </div>
        )}
      </div>
    </div>
  );
};

export default HistoryEventItem;