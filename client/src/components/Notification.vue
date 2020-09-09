<template>
  <div class="card" :style="cardStyle">
      <div class="columns">
          <div class="column">
              <p id="message">{{notification.message}}</p>
              <p id="date">{{dateFormat(notification.timeStamp)}}</p>
          </div>
          <b-button id="viewButton"
                    class="is-pulled-right"
                    @click="goToActivity(notification.activityId)">
              View Activity
          </b-button>
      </div>
  </div>
</template>

<script>
import toastMixin from "../mixins/toastMixin";
import store from "../store";
import router from "../router";
import dateTimeMixin from "../mixins/dateTimeMixin";

/**
 * Notification types that are classed as activity changes and the associated colour.
 * @type {{types: number[], colour: string}}
 */
const ACTIVITY_CHANGE = {
    types: [0, 1, 2, 5, 6],
    colour: "#a6b5ff"};
/**
 * Notification types that are classed as member changes and the associated colour.
 * @type {{types: number[], colour: string}}
 */
const MEMBER_CHANGE = {
    types: [3, 4, 7, 8, 9, 10],
    colour: "#99ff94"};

/**
 * The colour for notifications caused by the currently logged in user.
 * @type {string}
 */
const OWN_CHANGE_COLOUR = "#ffffb0";

/**
 * The default notification colour.
 * @type {string}
 */
const DEFAULT_COLOUR = "#ffffff";

Object.freeze(ACTIVITY_CHANGE);
Object.freeze(MEMBER_CHANGE);

export default {
    name: "Notification",
    mixins: [toastMixin, dateTimeMixin],
    data() {
        return {
            store: store,
            cardStyle: {
                backgroundColor: null
            }
        }
    },
    props: {
        notification: {
          type: Object,
          required: true
        }
    },
    /**
     * Sets the notification colour based on the notification type.
     */
    mounted() {
      if (this.isOwnNotification) {
          this.cardStyle.backgroundColor = OWN_CHANGE_COLOUR;
      } else if (ACTIVITY_CHANGE.types.includes(this.$props.notification.notificationType)) {
          this.cardStyle.backgroundColor = ACTIVITY_CHANGE.colour;
      } else if (MEMBER_CHANGE.types.includes(this.$props.notification.notificationType)) {
        this.cardStyle.backgroundColor = MEMBER_CHANGE.colour;
      } else {
        this.cardStyle.backgroundColor = DEFAULT_COLOUR
      }
    },
    methods: {
        goToActivity(id) {
            router.push({path: 'Activities/' + id})
        },
    },
    computed: {
        /**
         * Returns true if the notification was created by the currently logged in user; false otherwise
         */
        isOwnNotification() {
            return this.$props.notification.profileId === this.$store.getters.getUserId
        }
    }
}
</script>

<style scoped>
  .card{
    padding: 1rem;
  }

  .is-vertical-center {
      display: flex;
      align-items: center;
      padding-right: 1rem;
  }

</style>

